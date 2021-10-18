package com.pk.server;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.security.InvalidAlgorithmParameterException;
import java.util.Iterator;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import com.pk.server.models.Invite;
import com.pk.server.models.Player;

import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class BasicTcpServer implements TcpServer {
  private @Getter @Setter @NonNull BlockingQueue<Invite> bQueue;
  private Selector selector;
  private ServerSocketChannel serverSocketChannel;
  private @Setter @NonNull String nick;
  private @Setter @NonNull String profileImg;

  /**
   * Creates instance of BasicTcpServer, configures selector and binds to the
   * port.
   * 
   * @param bQueue queue which will be populated with received invitations
   * @param ip     ip to bind to
   * @param port   port to bind to
   * @throws IOException placeholder
   */
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
   * @throws Exception placeholder
   */
  @Override
  public Integer call() throws Exception {
    SelectionKey key = null;
    while (true) {
      if (Thread.currentThread().isInterrupted()) {
        return 0;
      }
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
  }

  public GameSession invite(Player player) throws InvalidAlgorithmParameterException, IOException {
    Socket sock = openSocketToPlayer(player.getIp());

    if (!sock.isConnected()) {
      log.warn("Socket is already closed");
      throw new InvalidAlgorithmParameterException();
    }
    sock.getOutputStream().write(("checkers:invitationAsk " + nick + " " + profileImg).getBytes());
    byte[] buf = new byte[100];
    int len = sock.getInputStream().read(buf);
    log.debug("Send len: " + len);
    String msg = new String(buf, 0, len);
    if (msg.equals("checkers:invitationOk")) {
      log.info("Invitation accepted");
      return new BasicGameSession(sock, new LinkedBlockingQueue<>(), new LinkedBlockingQueue<>());
    } else if (msg.equals("checkers:invitationRejected")) {
      throw new InvalidAlgorithmParameterException("Invitation rejected");
    } else {
      throw new InvalidAlgorithmParameterException("Invalid response: " + msg);
    }
  }

  /**
   * Create TCP session to remote machine.
   * 
   * @param addr address to connect to.
   * @return socket connected to player.
   * @throws IOException thrown when addr points to invalid location or network is
   *                     not available.
   */
  protected Socket openSocketToPlayer(InetAddress addr) throws IOException {
    return new Socket(addr, 10000);
  }

  public GameSession acceptInvitation(Invite invite) throws InvalidAlgorithmParameterException, IOException {
    Socket sock = invite.getSock();
    if (!sock.isConnected()) {
      log.warn("Socket is already closed");
      throw new InvalidAlgorithmParameterException();
    }
    // checkers:invitationOk
    sock.getOutputStream().write("checkers:invitationOk".getBytes());
    return new BasicGameSession(sock, new LinkedBlockingQueue<>(), new LinkedBlockingQueue<>());
  }

  /**
   * Method used by main TCP loop to read received message and verify whether it
   * is invitation or not.
   * 
   * @param key    placeholder
   * @param bQueue queue where new invites are stored.
   * @throws IOException placeholder
   */
  protected void readInvite(SelectionKey key, BlockingQueue<Invite> bQueue) throws IOException {
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
    if (addNewInvite(key, result, sc, bQueue)) {
      key.cancel();
    }
  }

  /**
   * Parse network message if correct type, extract nickname and profilePicture
   * encoded in base64 and add it to invite queue.
   * 
   * @param msg message received from interface.
   * @param sc  socket from which message arrived.
   * @return whether message was indeed invitation.
   * @throws IOException
   */
  private boolean addNewInvite(SelectionKey key, String msg, SocketChannel sc, BlockingQueue<Invite> bQueue)
      throws IOException {
    try {
      if (!msg.substring(0, 9).equals("checkers:")) {
        log.warn("Invalid substring: ", msg.substring(0, 9));
        return false;
      }
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
      log.info("pre");
      key.cancel();
      sc.configureBlocking(true);
      log.info("post");
      Invite invite = new Invite(items[0], items[1], sc.socket());
      log.info("Adding: " + invite.toString());
      bQueue.add(invite);
      return true;
    } catch (IndexOutOfBoundsException e) {
      log.warn("Exception: ", e);
      return false;
    }
  }
}
