package com.pk.lanserver;

import com.pk.lanserver.exceptions.InvitationRejected;
import com.pk.lanserver.exceptions.MoveRejected;
import com.pk.lanserver.models.Connection;
import com.pk.lanserver.models.Invite;
import com.pk.lanserver.models.Move;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketAddress;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.Arrays;
import java.util.Base64;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import lombok.NonNull;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class LanTcpServer implements LocalTcpServer {
  private SocketChannel remoteChannel;
  private BlockingQueue<Invite> invites;
  private BlockingQueue<String> messages;
  private BlockingQueue<Move> moves;
  private Selector selector;
  private ServerSocketChannel serverSocketChannel;
  private @Setter @NonNull String nick;
  private @Setter @NonNull String profileImg;
  private CompletableFuture<Boolean> futureInvite = null;
  // invCode -> Connection
  private Map<String, Connection> mapInvToConn;
  private String localIp;
  private Integer remotePort;

  /**
   * Creates instance of BasicTcpServer, configures selector and binds to the port.
   *
   * @param bQueue queue which will be populated with received invitations
   * @param ip ip to bind to
   * @param port port to bind to
   * @throws IOException placeholder
   */
  public LanTcpServer(
      BlockingQueue<Invite> invites,
      BlockingQueue<String> messages,
      BlockingQueue<Move> moves,
      String localIp,
      Integer localPort,
      Integer remotePort,
      String nick,
      String profileImg,
      Map<String, Connection> mapInvToConn)
      throws IOException {
    this.invites = invites;
    this.messages = messages;
    this.moves = moves;
    selector = Selector.open();
    serverSocketChannel = ServerSocketChannel.open();
    serverSocketChannel.configureBlocking(false);
    serverSocketChannel.bind(new InetSocketAddress(localPort));
    serverSocketChannel.register(selector, SelectionKey.OP_ACCEPT);
    this.profileImg = profileImg;
    this.nick = nick;
    this.localIp = localIp;
    this.remotePort = remotePort;
    this.mapInvToConn = mapInvToConn;
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
        log.error("Tcp thread interrupted");
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
          log.info("Got msg <{}>", msg);
          for (String packet : parseMessages(msg)) {
            innerCall(packet, key, sc);
          }
        } else if (key.isConnectable()) {
          log.error("Connectable, ???");
        }
      }
    }
  }

  private void innerCall(String msg, SelectionKey key, SocketChannel sc) throws IOException {
    if (msg.startsWith("checkers:msg ")) {
      log.info("Got CHAT type");
      messages.add(msg.substring(13));
    } else if (msg.startsWith("checkers:move ")) {
      log.info("Got MOVE type");
      moves.add(Move.fromString(msg.substring(14)));
    } else if (msg.startsWith("checkers:invitationAsk ")) {
      if (addNewInvite(key, msg, sc, invites)) {
        // TODO ?
        ;
      }
    } else if (msg.startsWith("checkers:inviteRejected")) {
      futureInvite.complete(false);
    } else if (msg.startsWith("checkers:inviteOk")) {
      futureInvite.complete(true);
      remoteChannel = sc;
    } else {
      log.warn("Got unknown message: " + msg);
    }
  }

  private List<String> parseMessages(String msg) {
    if (msg.charAt(msg.length() - 1) == '!') {
      msg = msg.substring(0, msg.length() - 1);
    }
    return Arrays.asList(msg.split("!"));
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
    futureInvite = new CompletableFuture<>();
    SocketAddress sockaddr = new InetSocketAddress(inviteCodeToIp(inviteCode), remotePort);
    SocketChannel scNew = SocketChannel.open();
    scNew.configureBlocking(true);
    log.info("Connect ret: {}", scNew.connect(sockaddr));
    scNew.write(ByteBuffer.wrap(String.format("checkers:invitationAsk %s %s %s", nick, profileImg, inviteCode).getBytes()));
    scNew.configureBlocking(false);
    selector.wakeup();
    scNew.register(selector, SelectionKey.OP_READ);
    log.info("scNew connected");
    return futureInvite;
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
    return new Socket(addr, remotePort);
  }

  public boolean acceptInvitation(String inviteCode) throws IOException {
    Connection conn = mapInvToConn.get(inviteCode);
    SocketChannel sc = conn.getSc();
    remoteChannel = sc;
    if (!remoteChannel.isConnected()) {
      log.error("Socket is already closed");
      throw new IOException("Socket is already closed");
    }
    remoteChannel.write(ByteBuffer.wrap(Utils.wrapMsg("inviteOk")));
    return true;
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
      Invite invite = new Invite(items[0], items[1], null, items[2]);
      mapInvToConn.put(items[2], new Connection(key, sc));
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
    if (remoteChannel == null) {
      log.error("Move remotePlayer is null");
      throw new IOException("remotePlayer is null");
    }
    remoteChannel.write(ByteBuffer.wrap(Utils.wrapMsg("move " + move.toSendableFormat())));
  }

  @Override
  public void chatSendMsg(String msg) throws IOException {
    if (remoteChannel == null) {
      log.error("Move remotePlayer is null");
      throw new IOException("remotePlayer is null");
    }
    byte[] encodedMsg = Base64.getEncoder().encode((msg).getBytes());
    remoteChannel.write(ByteBuffer.wrap(("checkers:msg " + new String(encodedMsg) + "!").getBytes()));
  }
}
