package com.pk;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.nio.channels.SocketChannel;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

import org.apache.commons.collections4.BidiMap;

import lombok.AllArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Setter
@AllArgsConstructor
public class SessionHandler implements Runnable {
  private String firstNick;
  private String secondNick;
  private Socket sFirst;
  private Socket sSecond;
  private BidiMap<String, SocketChannel> connectedPlayersMap;

  @Override
  public void run() {
    // FIXME find better naming pattern for this variables
    // FIXME magic number
    InputStream iStreamFirst;
    InputStream iStreamSecond;
    OutputStream oStreamFirst;
    OutputStream oStreamSecond;
    try {
      sFirst.setSoTimeout(100);
      sSecond.setSoTimeout(100);
      iStreamFirst = sFirst.getInputStream();
      iStreamSecond = sSecond.getInputStream();
      oStreamFirst = sFirst.getOutputStream();
      oStreamSecond = sSecond.getOutputStream();
    } catch (IOException e) {
      log.error("Got IOException while creating streams", e);
      return;
    }
    byte[] buf = new byte[100];
    for (;;) {
      if (!trySocket(iStreamFirst, oStreamSecond, firstNick, buf, 1)) {
        log.error("First socket is dead, terminating");
      }
      if (!trySocket(iStreamSecond, oStreamFirst, secondNick, buf, 2)) {
        log.error("Second socket is dead, terminating");
      }
    }
  }

  private boolean trySocket(InputStream in, OutputStream out, String nick, byte[] buf, int idx) {
    int timeouts = 0;
    int errors = 0;
    for (;;) {
      if (timeouts == 2) {
        return true;
      } else if (errors == 2) {
        return false;
      }
      try {
        int len = in.read(buf);
        // Connection closed, send termination msg
        if (len == 0) {
          log.info("PL_1");
          out.write("ERROR".getBytes(), 0, "ERROR".length());
          return false;
        }
        out.write(buf, 0, len);
      } catch (SocketTimeoutException ignore) {
        timeouts++;
      } catch (IOException | IndexOutOfBoundsException e) {
        errors++;
        Optional<Socket> sock = verifyHostIsUp(nick);
        if (sock.isEmpty()) {
          // FIXME magic number
          log.error("After 30 seconds host is still dead, sending msg to other side, and terminating");
          try {
            // FIXME placeholder
            out.write("ERROR".getBytes(), 0, "ERROR".length());
          } catch (IOException e1) {
            log.error("Both sides are dead, terminating");
          }
          return false;
        }
        // Set new socket, then try again
        if (idx == 1) {
          this.setSFirst(sock.get());
        } else {
          this.setSSecond(sock.get());
        }
      }
    }
  }

  private Optional<Socket> verifyHostIsUp(String nick) {
    // FIXME Magic number
    for (int i = 0; i < 30; ++i) {
      if (connectedPlayersMap.containsKey(nick)) {
        SocketChannel sc = connectedPlayersMap.get(nick);
        try {
          sc.configureBlocking(true);
        } catch (IOException e) {
          log.error("configureBlocking throw exception" ,e);
          return Optional.empty();
        }
        return Optional.of(sc.socket());
      }
      try {
        TimeUnit.SECONDS.sleep(1);
      } catch (Exception ignore) {}
    }
    return Optional.empty();
  }

}
