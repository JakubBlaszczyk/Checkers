package com.pk.server;

import java.io.IOException;
import java.net.Socket;
import java.util.Base64;
import java.util.concurrent.BlockingQueue;

import com.pk.server.exceptions.MoveRejected;
import com.pk.server.models.Move;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Getter
@AllArgsConstructor
public class BasicGameSession implements GameSession {
  private Socket socket;
  private BlockingQueue<String> bQueueMsgs;
  private BlockingQueue<Move> bQueueMoves;

  @Override
  public void move(Move move) throws IOException, MoveRejected {
    socket.getOutputStream().write(("checkers:move " + move.toSendableFormat()).getBytes());
    byte[] buf = new byte[100];
    for (;;) {
      int len = socket.getInputStream().read(buf);
      if (len == 0) {
        log.error("Invalid response");
      }
      String msg = new String(buf, 0, len);
      if (!msg.equals("checkers:moveOk")) {
        throw new MoveRejected("Packet received: " + msg);
      }
    }
  }

  @Override
  public void chatSendMsg(String msg) throws IOException {
    // checkers:msg MSG
    byte[] encodedMsg = Base64.getEncoder().encode(msg.getBytes());
    socket.getOutputStream().write(("checkers:msg " + new String(encodedMsg)).getBytes());
  }

  @Override
  public Integer call() throws Exception {
    byte[] buf = new byte[100];
    for (;;) {
      int len = socket.getInputStream().read(buf);
      log.info("len: " + len);
      if (len == -1) {
        // FIXME add clean up function
        throw new IOException("Connection closed");
      }
      String msg = new String(buf, 0, len);
      MsgType type = getType(msg);
      if (type == MsgType.CHAT) {
        log.info("Got CHAT type");
        bQueueMsgs.add(msg.substring(13));
      } else if (type == MsgType.MOVE) {
        log.info("Got MOVE type");
        bQueueMoves.add(Move.fromString(msg.substring(14)));
      }
    }
  }

  enum MsgType {
    CHAT, MOVE, UNKNOWN
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

}
