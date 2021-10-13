package com.pk.server;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;
import java.util.Base64;

import com.pk.server.exceptions.ChatMsgRejected;
import com.pk.server.exceptions.MoveRejected;
import com.pk.server.models.Move;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@AllArgsConstructor
public class BasicGameSession implements GameSession {
  private SocketChannel sc;

  @Override
  public void move(Move move) throws IOException, MoveRejected {
    // checkers:move srcX srcY dstX dstY
    sc.write(ByteBuffer.wrap(("checkers:move " + move.toSendableFormat()).getBytes()));
    ByteBuffer buf = ByteBuffer.wrap(new byte[100]);
    for (int i = 0; i < 99999; ++i) {
      if (sc.read(buf) == 0) {
        log.error("Invalid response");
      }
      String msg = new String(buf.array());
      if (!msg.substring(0, 13).equals("checkers:mov")) {
        continue;
      }
      if (!msg.equals("checkers:moveOk")) {
        throw new MoveRejected("Packet received: " + msg);
      }
    }
  }

  @Override
  public void chatSendMsg(String msg) throws IOException, ChatMsgRejected {
    // checkers:msg MSG
    byte[] encodedMsg = Base64.getEncoder().encode(msg.getBytes());
    sc.write(ByteBuffer.wrap(("checkers:msg " + new String(encodedMsg)).getBytes()));
    ByteBuffer buf = ByteBuffer.wrap(new byte[100]);
    for (int i = 0; i < 99999; ++i) {
      if (sc.read(buf) == 0) {
        log.error("Invalid response");
      }
      msg = new String(buf.array());
      if (!msg.substring(0, 13).equals("checkers:msg")) {
        continue;
      }
      if (!msg.equals("checkers:msgOk")) {
        throw new ChatMsgRejected("Packet received: " + msg);
      }
    }
  }

}
