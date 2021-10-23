package com.pk.server;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.Iterator;
import java.util.concurrent.BlockingQueue;

import com.pk.server.LanTcpServer.MsgType;
import com.pk.server.exceptions.InvitationRejected;
import com.pk.server.exceptions.MoveRejected;
import com.pk.server.models.Invite;
import com.pk.server.models.Move;
import com.pk.server.models.Player;

import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class WebTcpServer implements TcpServer {
  private Socket remotePlayer;
  private BlockingQueue<String> bQueueMsgs;
  private BlockingQueue<Move> bQueueMoves;
  private @Getter @Setter @NonNull BlockingQueue<Invite> bQueue;
  private Selector selector;
  private ServerSocketChannel serverSocketChannel;
  private @Setter @NonNull String nick;
  private @Setter @NonNull String profileImg;

  public WebTcpServer(BlockingQueue<Invite> bQueue, String ip, Integer port) throws IOException {
    this.bQueue = bQueue;
    selector = Selector.open();
    serverSocketChannel = ServerSocketChannel.open();
    serverSocketChannel.configureBlocking(false);
    serverSocketChannel.bind(new InetSocketAddress(ip, port));
    serverSocketChannel.register(selector, SelectionKey.OP_ACCEPT);
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
          MsgType type = getType(msg);
          if (type == MsgType.CHAT) {
            log.info("Got CHAT type");
            bQueueMsgs.add(msg.substring(13));
          } else if (type == MsgType.MOVE) {
            log.info("Got MOVE type");
            bQueueMoves.add(Move.fromString(msg.substring(14)));
          } else if (type == MsgType.UNKNOWN) {
            log.warn("Got unknown message: " + msg);
          }
          readInvite(key, bQueue);
        }
      }
    }
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

  private MsgType getType(String msg) {
    log.info("Got msg: " + msg);
    try {
      if (msg.substring(0, 13).equals("checkers:msg ")) {
        return MsgType.CHAT;
      } else if (msg.substring(0, 14).equals("checkers:move ")) {
        return MsgType.MOVE;
      }
    } catch (Exception e) {
      log.info("Exception msg: " + e.getMessage());
    }
    return MsgType.UNKNOWN;
  }

  @Override
  public boolean invite(Player invite) throws InvitationRejected, IOException {
    remotePlayer.getOutputStream().write(("checkers:inviteAsk " + invite.getNick()).getBytes());
    return false;
  }

  @Override
  public boolean acceptInvitation(Invite invite) throws IOException {
    remotePlayer.getOutputStream().write(("checkers:inviteOk").getBytes());
    return false;
  }

  @Override
  public void cleanup() throws IOException {
    serverSocketChannel.close();
    selector.close();
  }

  @Override
  public void move(Move move) throws IOException, MoveRejected {
    remotePlayer.getOutputStream().write(("checkers:move " + move.toSendableFormat()).getBytes());
    byte[] tmp = new byte[100];
    int len = remotePlayer.getInputStream().read(tmp);
    if (len == 0) {
      throw new IOException("Connection closed");
    }
    String msg = new String(tmp, 0, len);
    if (!msg.equals("checkers:moveOk")) {
      throw new MoveRejected("Invalid response: " + msg);
    }
  }

  @Override
  public void chatSendMsg(String msg) throws IOException {
    remotePlayer.getOutputStream().write(msg.getBytes());
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
