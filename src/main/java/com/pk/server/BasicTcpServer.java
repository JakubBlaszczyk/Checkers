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

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

/**
 * Class used to perform all actions on Tcp stream
 */
@Slf4j
@Getter
@Setter
@AllArgsConstructor
public class BasicTcpServer implements TcpServer {
  private BlockingQueue<Invite> bQueue;
  private String ip;
  private Integer port;

  /**
   * Thread used to handle all incoming Tcp traffic
   */
  @Override
  public void run() {
    try (Selector selector = Selector.open(); ServerSocketChannel serverSocketChannel = ServerSocketChannel.open()) {
      serverSocketChannel.configureBlocking(false);
      serverSocketChannel.bind(new InetSocketAddress("0.0.0.0", 10000));
      serverSocketChannel.register(selector, SelectionKey.OP_ACCEPT);
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
            SocketChannel sc = (SocketChannel) key.channel();
            ByteBuffer bb = ByteBuffer.allocate(1024);
            int len = sc.read(bb);
            if (len <= 0) {
              sc.close();
              log.info("Connection closed");
              continue;
            }
            String result = new String(bb.array(), 0, len);
            log.info("Message received: " + result + " Message length: " + result.length());
            if (addNewInvite(result, sc)) {
              key.cancel();
            }
          }
        }
      }
    } catch (Exception e) {
      log.error("ERROR: ", e);
    }
  }
  /**
   * Parse network message if correct type, extract nickname and profilePicture encoded in base64 and add it to invite queue
   * @param msg Message received from interface
   * @param sc Socket from which message arrived
   * @return Whether message was indeed invitation 
   */
  private boolean addNewInvite(String msg, SocketChannel sc) {
    try {
      if (!msg.substring(0, 9).equals("checkers:")) {
        log.warn("Invalid substring: ", msg.substring(0, 9));
        return false;
      }
      // checkers:invitationAsk nick profileImg
      msg = msg.substring(9);
      if (!msg.substring(0, 13).equals("invitationAsk")) {
        log.info("Invalid substring: ", msg.substring(0, 13));
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

  /**
   * Method used to create GameSession to specified player.
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
    String msg = new String(buf.array());
    if (msg.equals("checkers:invitationOk")) {
      log.info("Invitation accepted");
      return new BasicGameSession(sc);
      // FIXME add exceptions
    } else if (msg.equals("checkers:invitationRejected")) {
      log.info("Invitation rejected");
      return null;
    } else {
      log.info("Invalid response: " + msg);
      return null;
    }
  }
  
  /**
   * Method used to create GameSession to specified player.
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
}
