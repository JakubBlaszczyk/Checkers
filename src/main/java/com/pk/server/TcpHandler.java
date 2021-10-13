package com.pk.server;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.SocketChannel;
import java.util.concurrent.BlockingQueue;

import com.pk.server.models.Invite;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class TcpHandler {
  public int sendPacket(SocketChannel sc, ByteBuffer msg) throws IOException {
    return sc.write(msg);
  }

  public int receivePacket(SocketChannel sc, ByteBuffer buf) throws IOException {
    return sc.read(buf);
  }

  public void readMsg(SelectionKey key, BlockingQueue<Invite> bQueue) throws IOException {
    SocketChannel sc = (SocketChannel) key.channel();
    ByteBuffer bb = ByteBuffer.allocate(1024);
    int len = receivePacket(sc, bb);
    if (len <= 0) {
      sc.close();
      log.info("Connection closed");
      return;
    }
    String result = new String(bb.array(), 0, len);
    log.info("Message received: " + result + " Message length: " + result.length());
    if (addNewInvite(result, sc, bQueue)) {
      key.cancel();
    }
  }

  /**
   * Parse network message if correct type, extract nickname and profilePicture
   * encoded in base64 and add it to invite queue
   * 
   * @param msg Message received from interface
   * @param sc  Socket from which message arrived
   * @return Whether message was indeed invitation
   */
  private boolean addNewInvite(String msg, SocketChannel sc, BlockingQueue<Invite> bQueue) {
    try {
      if (!msg.substring(0, 9).equals("checkers:")) {
        log.warn("Invalid substring: ", msg.substring(0, 9));
        return false;
      }
      // checkers:invitationAsk nick profileImg
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
}
