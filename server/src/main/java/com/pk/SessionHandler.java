package com.pk;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.nio.channels.SocketChannel;
import lombok.AllArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.BidiMap;

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
    for (; ; ) {
      if (!trySocket(iStreamFirst, oStreamSecond, buf)) {
        log.error("First socket is dead, terminating");
        return;
      }
      if (!trySocket(iStreamSecond, oStreamFirst, buf)) {
        log.error("Second socket is dead, terminating");
        return;
      }
    }
  }

  private boolean trySocket(InputStream in, OutputStream out, byte[] buf) {
    while (true) {
      try {
        int len = in.read(buf);
        log.info("Got msg len: {}", len);
        // Connection closed, send termination msg
        if (len == 0) {
          log.info("trySocket failed");
          out.write("ERROR".getBytes(), 0, "ERROR".length());
          return false;
        }
        log.info("Msg: {}", new String(buf, 0, len));
        out.write(buf, 0, len);
        return true;
      } catch (SocketTimeoutException ignore) {
        log.trace("trySocket timeout");
      } catch (Exception e) {
        log.error("trySocket error, {}", e);
        return false;
      }
    }
  }
}
