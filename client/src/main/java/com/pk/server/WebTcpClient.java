package com.pk.server;

import com.pk.server.exceptions.InvitationRejected;
import com.pk.server.exceptions.MoveRejected;
import com.pk.server.models.Invite;
import com.pk.server.models.Move;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.Arrays;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;
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
  private String inviteCode;
  // 0 - waiting, -1 - rejected, 1 - accepted
  private @Getter int inviteAccepted;

  public WebTcpClient(
      BlockingQueue<Invite> bQueueInvites, BlockingQueue<String> bQueueMsgs, BlockingQueue<Move> bQueueMoves, String nick, String profileImg)
      throws IOException {
    this.bQueueInvites = bQueueInvites;
    this.bQueueMsgs = bQueueMsgs;
    this.bQueueMoves = bQueueMoves;
    this.nick = nick;
    this.profileImg = profileImg;
    this.inviteAccepted = 0;
  }

  /**
   * Blocking method used to receive invitation code. Non blocking one can be achieved using inviteCode getter.
   * @return
   */
  public String blockGetCode() {
    while (true) {
      try {
        Thread.sleep(100);
      } catch (InterruptedException ignore) {
        return null;
      }
      if (!inviteCode.equals(""))
        return inviteCode;
    }
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
      String msg = new String(buf, 0, len).strip();
      log.info("Got msg: " + msg);
      if (msg.equals("checkers:Hello")) {
        os.write(String.format("checkers:config %s %s", nick, profileImg).getBytes());
      } else if (msg.startsWith("checkers:confOk ")) {
        log.info("Got config - OK, invite code: " + msg.substring(16));
        inviteCode = msg.substring(16);
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
        this.inviteAccepted = 1;
      } else if (msg.startsWith("checkers:inviteRejected ")) {
        this.inviteAccepted = -1;
      } else {
        log.warn("Got unknown message: " + msg);
      }
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

  public boolean invite(String inviteCode) throws InvitationRejected, IOException {
    remotePlayer.getOutputStream().write(("checkers:inviteAsk " + inviteCode).getBytes());
    // byte[] buf = new byte[100];
    // int len = remotePlayer.getInputStream().read(buf);
    // if (len == 0) {
    //   log.warn("Connection closed in invite handling???");
    //   return false;
    // }
    return true;
  }

  public boolean acceptInvitation(String inviteCode) throws IOException {
    remotePlayer.getOutputStream().write(("checkers:inviteOk " + inviteCode).getBytes());
    return true;
  }

  public void cleanup() throws IOException {
    remotePlayer.close();
  }

  public void move(Move move) throws IOException, MoveRejected {
    remotePlayer.getOutputStream().write(("checkers:move " + move.toSendableFormat()).getBytes());
    // byte[] tmp = new byte[100];
    // int len = remotePlayer.getInputStream().read(tmp);
    // if (len == 0) {
    //   throw new IOException("Connection closed");
    // }
    // String msg = new String(tmp, 0, len);
    // if (!msg.equals("checkers:moveOk")) {
    //   throw new MoveRejected("Invalid response: " + msg);
    // }
  }

  public void chatSendMsg(String msg) throws IOException {
    remotePlayer.getOutputStream().write(("checkers:chat " + msg).getBytes());
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

  public String getInviteCode() {
    return inviteCode;
  }
}
