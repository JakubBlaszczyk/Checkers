package com.pk.server;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.security.InvalidAlgorithmParameterException;
import java.util.Iterator;
import java.util.Set;
import java.util.concurrent.BlockingQueue;

import com.pk.server.models.Invite;

import lombok.Cleanup;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

/**
 * Class used to perform all actions on Tcp stream
 */
@Slf4j
public class BasicTcpServer implements TcpServer {
  private @Getter @Setter @NonNull BlockingQueue<Invite> bQueue;
  private Selector selector;
  private ServerSocketChannel serverSocketChannel;

  public BasicTcpServer(BlockingQueue<Invite> bQueue, String ip, Integer port) throws IOException {
    this.bQueue = bQueue;
    selector = Selector.open();
    serverSocketChannel = ServerSocketChannel.open();
    serverSocketChannel.configureBlocking(false);
    serverSocketChannel.bind(new InetSocketAddress(ip, port));
    serverSocketChannel.register(selector, SelectionKey.OP_ACCEPT);
  }

  public void cleanup() throws IOException {
    serverSocketChannel.close();
    selector.close();
  }

  /**
   * Thread used to handle all incoming Tcp traffic
   */
  @Override
  public void run() {
    try {
      SelectionKey key = null;
      while (true) {
        if (selector.select() <= 0)
          continue;
        Set<SelectionKey> selectedKeys = selector.selectedKeys();
        Iterator<SelectionKey> iterator = selectedKeys.iterator();
        while (iterator.hasNext()) {
          key = iterator.next();
          iterator.remove();
          if (key.isAcceptable()) {
            SocketChannel sc = serverSocketChannel.accept();
            sc.configureBlocking(false);
            sc.register(selector, SelectionKey.OP_READ);
            log.info("Connection Accepted: " + sc.getLocalAddress());
          }
          if (key.isReadable()) {
            readInvite(key, bQueue);
          }
        }
      }
    } catch (Exception e) {
      log.error("ERROR: ", e);
    }
  }

  /**
   * Method used to create GameSession to specified player.
   * 
   * @param invite Selected players invite
   * @return new instance of GameSession
   */
  public GameSession invite(Invite invite) throws InvalidAlgorithmParameterException, IOException {
    SocketChannel sc = invite.getSc();
    if (!(sc.isConnected() && sc.isOpen())) {
      log.warn("Socket is already closed");
      throw new InvalidAlgorithmParameterException();
    }
    if (!sc.isBlocking()) {
      sc.configureBlocking(true);
    }
    // checkers:invitationOk
    sc.write(ByteBuffer.wrap("checkers:invitationAsk".getBytes()));
    ByteBuffer buf = ByteBuffer.wrap(new byte[100]);
    sc.read(buf);
    String msg = new String(buf.array(), 0, getBytesLen(buf));
    if (msg.equals("checkers:invitationOk")) {
      log.info("Invitation accepted");
      return new BasicGameSession(sc);
    } else if (msg.equals("checkers:invitationRejected")) {
      throw new InvalidAlgorithmParameterException("Invitation rejected");
    } else {
      throw new InvalidAlgorithmParameterException("Invalid response: " + msg);
    }
  }

  public static int getBytesLen(ByteBuffer bb) {
    int len = 0;
    try {
      byte[] arr = bb.array();
      for (int i = 0;; ++i) {
        int x = arr[i];
        log.debug("ARR: " + x);
        if (x != 0) {
          len += 1;
          continue;
        }
        return len;
      }
    } catch (Exception e) {
      return len;
    }
  }

  /**
   * Method used to create GameSession to specified player.
   * 
   * @param invite Selected players invite
   * @return new instance of GameSession
   */
  public GameSession acceptInvitation(Invite invite) throws InvalidAlgorithmParameterException, IOException {
    SocketChannel sc = invite.getSc();
    if (!(sc.isConnected() && sc.isOpen())) {
      log.warn("Socket is already closed");
      throw new InvalidAlgorithmParameterException();
    }
    if (!sc.isBlocking()) {
      sc.configureBlocking(true);
    }
    // checkers:invitationOk
    sc.write(ByteBuffer.wrap("checkers:invitationOk".getBytes()));
    return new BasicGameSession(sc);
  }

  public void readInvite(SelectionKey key, BlockingQueue<Invite> bQueue) throws IOException {
    SocketChannel sc = (SocketChannel) key.channel();
    ByteBuffer bb = ByteBuffer.allocate(1024);
    int len = sc.read(bb);
    if (len <= 0) {
      sc.close();
      log.info("Connection closed");
      return;
    }
    String result = new String(bb.array(), 0, len);
    log.info("Message received: " + result + " Message length: " + result.length());
    if (addNewInvite(result, sc, bQueue)) {
      key.cancel();
    }
  }

  /**
   * Parse network message if correct type, extract nickname and profilePicture
   * encoded in base64 and add it to invite queue
   * 
   * @param msg Message received from interface
   * @param sc  Socket from which message arrived
   * @return Whether message was indeed invitation
   */
  private boolean addNewInvite(String msg, SocketChannel sc, BlockingQueue<Invite> bQueue) {
    try {
      if (!msg.substring(0, 9).equals("checkers:")) {
        log.warn("Invalid substring: ", msg.substring(0, 9));
        return false;
      }
      // checkers:invitationAsk nick profileImg
      msg = msg.substring(9);
      if (!msg.substring(0, 14).equals("invitationAsk ")) {
        log.info("Invalid substring: ", msg.substring(0, 14));
        return false;
      }
      String[] items = msg.substring(14).split(" ");
      if (items.length != 2) {
        log.warn("Invalid size: " + items.length + ", items: " + items[0]);
        return false;
      }
      log.info(String.valueOf(items.length));
      for (String item : items) {
        log.info(item);
      }
      // FIXME same info stored 2 times
      Invite invite = new Invite(sc.socket().getInetAddress(), items[0], items[1], sc);
      log.info(invite.toString());
      bQueue.add(invite);
      return true;
    } catch (IndexOutOfBoundsException e) {
      log.warn("Exception: ", e);
      return false;
    }
  }
}
