package com.pk.server;

import com.pk.server.exceptions.InvitationRejected;
import com.pk.server.exceptions.MoveRejected;
import com.pk.server.models.Invite;
import com.pk.server.models.Move;
import com.pk.server.models.Player;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class WebTcpClient implements Callable<Integer> {
  private @NonNull Socket remotePlayer;
  private @NonNull BlockingQueue<String> bQueueMsgs;
  private @NonNull BlockingQueue<Move> bQueueMoves;
  private @NonNull BlockingQueue<Invite> bQueueInvites;
  private @Setter @NonNull String nick;
  private @Setter @NonNull String profileImg;
  private CompletableFuture<String> futureInviteCode;
  private CompletableFuture<List<Player>> futurePlayersList;
  private CompletableFuture<Boolean> futureInviteAccepted;
  // 0 - waiting, -1 - rejected, 1 - accepted
  private @Getter int inviteAccepted;

  public WebTcpClient(
      BlockingQueue<Invite> bQueueInvites,
      BlockingQueue<String> bQueueMsgs,
      BlockingQueue<Move> bQueueMoves,
      String addr,
      Integer port,
      String nick,
      String profileImg)
      throws IOException {
    this.bQueueInvites = bQueueInvites;
    this.bQueueMsgs = bQueueMsgs;
    this.bQueueMoves = bQueueMoves;
    this.nick = nick;
    this.profileImg = profileImg;
    remotePlayer = new Socket(InetAddress.getByName(addr), port);
    futureInviteCode = new CompletableFuture<>();
    futurePlayersList = null;
    futureInviteAccepted = null;
  }

  private List<String> parseMessages(String msg) {
    if (msg.charAt(msg.length() - 1) == '!') {
      msg = msg.substring(0, msg.length() - 1);
    }
    return Arrays.asList(msg.split("!"));
  }

  /** @throws Exception placeholder */
  @Override
  public Integer call() throws Exception {
    InputStream is = remotePlayer.getInputStream();
    OutputStream os = remotePlayer.getOutputStream();
    while (true) {
      if (Thread.currentThread().isInterrupted()) {
        log.info("Thread is interrupted");
        return 0;
      }
      byte[] buf = new byte[100];
      log.info("Waiting for msg");
      int len = is.read(buf);
      if (len <= 0) {
        remotePlayer.close();
        log.info("Connection closed");
        return 0;
      }
      String tmp = new String(buf, 0, len).strip();
      for (String msg : parseMessages(tmp)) {
        innerCall(msg, os);
      }
    }
  }

  private void innerCall(String msg, OutputStream os) throws IOException {
    log.info("Got msg: " + msg);
    if (msg.equals("checkers:Hello")) {
      os.write(Utils.wrapMsg(String.format("config %s %s", nick, profileImg)));
    } else if (msg.startsWith("checkers:confOk ")) {
      log.info("Got config - OK, invite code: " + msg.substring(16));
      futureInviteCode.complete(msg.substring(16));
    } else if (msg.startsWith("checkers:chat ")) {
      log.info("Got CHAT type");
      bQueueMsgs.add(msg.substring(14));
    } else if (msg.startsWith("checkers:move ")) {
      log.info("Got MOVE type");
      bQueueMoves.add(Move.fromString(msg.substring(14)));
    } else if (msg.startsWith("checkers:inviteAsk ")) {
      if (!addNewInvite(msg.substring(19), bQueueInvites)) {
        log.warn("Got invalid invitation, ignoring");
      }
    } else if (msg.startsWith("checkers:inviteOk ")) {
      futureInviteAccepted.complete(true);
    } else if (msg.startsWith("checkers:inviteRejected ")) {
      futureInviteAccepted.complete(false);
    } else if (msg.startsWith("checkers:onlinePlayers ")) {
      parseOnlinePlayers(msg.substring(23));
    } else {
      log.warn("Got unknown message: " + msg);
    }
  }

  private void parseOnlinePlayers(String players) {
    String[] items = players.split(" ");
    if (items.length % 2 != 0) {
      log.error("Error, invalid items size {}", items.length);
      return;
    }
    List<Player> parsed = new ArrayList<>();

    for (int i = 0; i < items.length; i += 2) {
      parsed.add(new Player(null, items[i], items[i + 1]));
    }
    futurePlayersList.complete(parsed);
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
  private boolean addNewInvite(String msg, BlockingQueue<Invite> bQueue) throws IOException {
    try {
      String[] items = msg.split(" ");
      if (items.length != 3) {
        log.warn("Invalid size: " + items.length + ", items: " + Arrays.toString(items));
        return false;
      }
      log.info(String.valueOf(items.length));
      for (String item : items) {
        log.info(item);
      }
      Invite invite = new Invite(items[0], items[1], remotePlayer, items[2]);
      log.info("Adding: " + invite.toString());
      bQueue.add(invite);
      return true;
    } catch (IndexOutOfBoundsException e) {
      log.warn("Exception: ", e);
      return false;
    }
  }

  public Future<Boolean> invite(String inviteCode) throws InvitationRejected, IOException {
    futureInviteAccepted = new CompletableFuture<>();
    remotePlayer.getOutputStream().write(Utils.wrapMsg("inviteAsk " + inviteCode));
    return futureInviteAccepted;
  }

  public Future<List<Player>> getActivePlayers() throws IOException {
    futurePlayersList = new CompletableFuture<>();
    remotePlayer.getOutputStream().write(Utils.wrapMsg("getPlayers"));
    return futurePlayersList;
  }

  public boolean acceptInvitation(String inviteCode) throws IOException {
    remotePlayer.getOutputStream().write(Utils.wrapMsg("inviteOk " + inviteCode));
    return true;
  }

  public void cleanup() throws IOException {
    remotePlayer.close();
  }

  public void move(Move move) throws IOException, MoveRejected {
    remotePlayer.getOutputStream().write(Utils.wrapMsg("move " + move.toSendableFormat()));
  }

  public void chatSendMsg(String msg) throws IOException {
    remotePlayer.getOutputStream().write(Utils.wrapMsg("chat " + msg));
  }

  public BlockingQueue<String> getBQueueMsgs() {
    return bQueueMsgs;
  }

  public BlockingQueue<Move> getBQueueMoves() {
    return bQueueMoves;
  }

  public Socket getSocket() {
    return remotePlayer;
  }

  public Future<String> getInviteCode() {
    return futureInviteCode;
  }
}
