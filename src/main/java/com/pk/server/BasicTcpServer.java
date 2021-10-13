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
  private TcpHandler tcpHandler;

  public BasicTcpServer(BlockingQueue<Invite> bQueue, String ip, Integer port) throws IOException {
    this.bQueue = bQueue;
    selector = Selector.open();
    serverSocketChannel = ServerSocketChannel.open();
    serverSocketChannel.configureBlocking(false);
    serverSocketChannel.bind(new InetSocketAddress(ip, port));
    serverSocketChannel.register(selector, SelectionKey.OP_ACCEPT);
    tcpHandler = new TcpHandler();
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
            tcpHandler.readMsg(key, bQueue);
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
    tcpHandler.sendPacket(sc, ByteBuffer.wrap("checkers:invitationAsk".getBytes()));
    ByteBuffer buf = ByteBuffer.wrap(new byte[100]);
    tcpHandler.receivePacket(sc, buf);
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
    tcpHandler.sendPacket(sc, ByteBuffer.wrap("checkers:invitationOk".getBytes()));
    return new BasicGameSession(sc);
  }
}
