package com.pk.lanserver;

import com.pk.lanserver.exceptions.InvitationRejected;
import com.pk.lanserver.exceptions.MoveRejected;
import com.pk.lanserver.models.Invite;
import com.pk.lanserver.models.Move;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.Base64;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class LanTcpServer implements LocalTcpServer {
  private Socket remotePlayer;
  private BlockingQueue<String> bQueueMsgs;
  private BlockingQueue<Move> bQueueMoves;
  private @Getter @Setter @NonNull BlockingQueue<Invite> bQueueInvites;
  private Selector selector;
  private ServerSocketChannel serverSocketChannel;
  private @Setter @NonNull String nick;
  private @Setter @NonNull String profileImg;
  // invCode -> Socket
  private Map<String, Socket> mapInvToSock;
  private String localIp;

  /**
   * Creates instance of BasicTcpServer, configures selector and binds to the port.
   *
   * @param bQueue queue which will be populated with received invitations
   * @param ip ip to bind to
   * @param port port to bind to
   * @throws IOException placeholder
   */
  public LanTcpServer(
      BlockingQueue<Invite> bQueueInvites,
      BlockingQueue<String> bQueueMsgs,
      BlockingQueue<Move> bQueueMoves,
      String ip,
      String localIp,
      Integer port,
      String nick,
      String profileImg,
      Map<String, Socket> mapInvToSock)
      throws IOException {
    this.bQueueInvites = bQueueInvites;
    this.bQueueMsgs = bQueueMsgs;
    this.bQueueMoves = bQueueMoves;
    selector = Selector.open();
    serverSocketChannel = ServerSocketChannel.open();
    serverSocketChannel.configureBlocking(false);
    serverSocketChannel.bind(new InetSocketAddress(port));
    serverSocketChannel.register(selector, SelectionKey.OP_ACCEPT);
    this.profileImg = profileImg;
    this.nick = nick;
    this.localIp = localIp;
  }

  public void cleanup() throws IOException {
    serverSocketChannel.close();
    selector.close();
  }

  /** @throws Exception placeholder */
  @Override
  public Integer call() throws Exception {
    SelectionKey key = null;
    while (true) {
      if (Thread.currentThread().isInterrupted()) {
        return 0;
      }
      if (selector.select() <= 0) continue;
      Iterator<SelectionKey> iterator = selector.selectedKeys().iterator();
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
          String msg = new String(bb.array(), 0, len).strip();
          if (msg.startsWith("checkers:msg ")) {
            log.info("Got CHAT type");
            bQueueMsgs.add(msg.substring(13));
          } else if (msg.startsWith("checkers:move ")) {
            log.info("Got MOVE type");
            bQueueMoves.add(Move.fromString(msg.substring(14)));
          } else {
            log.warn("Got unknown message: " + msg);
          }
          readInvite(key, bQueueInvites);
        }
      }
    }
  }

  private InetAddress inviteCodeToIp(String inv) throws UnknownHostException {
    return InetAddress.getByName(
        String.valueOf(Integer.parseInt(inv.substring(0, 3)))
            + "."
            + Integer.parseInt(inv.substring(3, 6))
            + "."
            + Integer.parseInt(inv.substring(6, 9))
            + "."
            + Integer.parseInt(inv.substring(9, 12)));
  }

  public Future<Boolean> invite(String inviteCode) throws InvitationRejected, IOException {
    Socket sock = openSocketToPlayer(inviteCodeToIp(inviteCode));

    if (!sock.isConnected()) {
      log.error("Socket is already closed");
      throw new IOException("Socket is already closed");
    }
    sock.getOutputStream()
        .write(Utils.wrapMsg("checkers:invitationAsk " + nick + " " + profileImg));
    byte[] buf = new byte[100];
    int len = sock.getInputStream().read(buf);
    log.debug("Send len: " + len);
    String msg = new String(buf, 0, len);
    if (msg.equals("checkers:invitationOk")) {
      log.info("Invitation accepted");
      remotePlayer = mapInvToSock.get(inviteCode);
      return CompletableFuture.completedFuture(true);
    } else if (msg.equals("checkers:invitationRejected")) {
      return CompletableFuture.completedFuture(false);
      // throw new InvitationRejected("Invitation rejected");
    } else {
      // TODO correct?
      return CompletableFuture.completedFuture(false);
      // throw new InvitationRejected("Invalid response: " + msg);
    }
  }

  /**
   * Create TCP session to remote machine.
   *
   * @param addr address to connect to.
   * @return socket connected to player.
   * @throws IOException thrown when addr points to invalid location or network is not available.
   */
  protected Socket openSocketToPlayer(InetAddress addr) throws IOException {
    log.info("TCP connecting to {}", addr.getHostAddress());
    return new Socket(addr, 10000);
  }

  public boolean acceptInvitation(String inviteCode) throws IOException {
    remotePlayer = mapInvToSock.get(inviteCode);
    if (!remotePlayer.isConnected()) {
      log.error("Socket is already closed");
      throw new IOException("Socket is already closed");
    }
    remotePlayer.getOutputStream().write(Utils.wrapMsg("invitationOk"));
    return true;
  }

  /**
   * Method used by main TCP loop to read received message and verify whether it is invitation or
   * not.
   *
   * @param key placeholder
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
   * Parse network message if correct type, extract nickname and profilePicture encoded in base64
   * and add it to invite queue.
   *
   * @param msg message received from interface.
   * @param sc socket from which message arrived.
   * @return whether message was indeed invitation.
   * @throws IOException
   */
  private boolean addNewInvite(
      SelectionKey key, String msg, SocketChannel sc, BlockingQueue<Invite> bQueue)
      throws IOException {
    try {
      if (!msg.startsWith("checkers:")) {
        log.warn("Invalid substring: ", msg.substring(0, 9));
        return false;
      }
      msg = msg.substring(9);
      if (!msg.startsWith("invitationAsk ")) {
        log.info("Invalid substring: ", msg.substring(0, 14));
        return false;
      }
      String[] items = msg.substring(14).split(" ");
      if (items.length != 3) {
        log.warn("Invalid size: {}", items.length);
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
      Invite invite = new Invite(items[0], items[1], sc.socket(), items[2]);
      log.info("Adding: " + invite.toString());
      bQueue.add(invite);
      return true;
    } catch (IndexOutOfBoundsException e) {
      log.warn("Exception: ", e);
      return false;
    }
  }

  public Future<String> getInviteCode() {
    return CompletableFuture.completedFuture(ipToInvCode());
  }

  private String ipToInvCode() {
    StringBuilder sb = new StringBuilder();
    for (String part : localIp.split("\\.")) {
      if (part.length() == 3) {
        sb.append(part);
      } else if (part.length() == 2) {
        sb.append("0" + part);
      } else {
        sb.append("00" + part);
      }
    }
    return sb.toString();
  }

  @Override
  public void move(Move move) throws IOException, MoveRejected {
    remotePlayer.getOutputStream().write((Utils.wrapMsg("move " + move.toSendableFormat())));
  }

  @Override
  public void chatSendMsg(String msg) throws IOException {
    byte[] encodedMsg = Base64.getEncoder().encode(msg.getBytes());
    remotePlayer.getOutputStream().write(("checkers:msg " + new String(encodedMsg)).getBytes());
  }

  @Override
  public BlockingQueue<String> getBQueueMsgs() {
    return bQueueMsgs;
  }

  @Override
  public BlockingQueue<Move> getBQueueMoves() {
    return bQueueMoves;
  }

  @Override
  public Socket getSocket() {
    return remotePlayer;
  }
}
