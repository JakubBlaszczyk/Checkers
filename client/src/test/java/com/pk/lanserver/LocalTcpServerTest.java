package com.pk.lanserver;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import com.pk.lanserver.models.Invite;
import com.pk.lanserver.models.Move;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.Socket;
import java.net.SocketException;
import java.util.Base64;
import java.util.Collections;
import java.util.HashMap;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Test;

@Slf4j
public class LocalTcpServerTest {
  private String inviteCode;
  private String localAddr = "";
  private Boolean failed = false;
  private BlockingQueue<Invite> invitesThread = new LinkedBlockingQueue<>();
  private BlockingQueue<String> messagesThread = new LinkedBlockingQueue<>();
  private BlockingQueue<Move> movesThread = new LinkedBlockingQueue<>();

  @Test
  public void testIfServerIsAcceptingConnections() throws Exception {
    BlockingQueue<Invite> invites = new LinkedBlockingQueue<>();
    BlockingQueue<String> messages = new LinkedBlockingQueue<>();
    BlockingQueue<Move> moves = new LinkedBlockingQueue<>();
    LocalTcpServer tcpServer =
        new LanTcpServer(invites, messages, moves, "127.0.0.1", 10000, 10000, "dDI=", "dDI=", new HashMap<>());
    ExecutorService executorService = Executors.newFixedThreadPool(1);
    Future<Integer> futureTcp = executorService.submit(tcpServer);
    Socket sock = new Socket(InetAddress.getByName("127.0.0.1"), 10000);
    assertTrue(sock.isConnected());
    futureTcp.cancel(true);
    tcpServer.cleanup();
    sock.close();
  }

  @Test
  public void testConnection() {
    try {
      NetworkInterface iff = NetworkInterface.getByName("eno1");
      if (iff == null) {
        iff = NetworkInterface.getByName("eth0");
      }
      for (InetAddress inetAddress : Collections.list(iff.getInetAddresses())) {
        log.info("InetAddress: %s%n", inetAddress.getHostAddress());
        if (inetAddress instanceof Inet4Address) {
          log.info("Found: {}", inetAddress.getHostAddress());
          localAddr = inetAddress.getHostAddress();
        }
      }
    } catch (SocketException e) {
      e.printStackTrace();
      fail("Got exception in network if search");
    }
    if (localAddr.isEmpty()) {
      return;
    }
    Thread one =
        new Thread() {
          public void run() {
            LanServerController wts = null;
            try {
              wts =
                  new LanServerController(
                      invitesThread,
                      messagesThread,
                      movesThread,
                      localAddr,
                      10000,
                      10001,
                      "dGVzdA==",
                      "dGVzdA==",
                      new HashMap<>());
              TimeUnit.SECONDS.sleep(3);
              inviteCode = wts.getInviteCode().get();
              log.info("Got invite code: {}", inviteCode);
              log.info("Got active players: {}", wts.getActivePlayers().get());
              for (; ; ) {
                TimeUnit.SECONDS.sleep(1);
                if (!invitesThread.isEmpty()) {
                  log.info("Found inv");
                  break;
                }
              }
              Invite inv = (Invite) invitesThread.poll();
              wts.acceptInvitation(inv.getCode());
              wts.move(new Move(1, 2, 3, 4));
              wts.chatSendMsg("tx1");
              TimeUnit.SECONDS.sleep(1000000);
              log.info("GET RET");
            } catch (InterruptedException e) {
              log.info("InterruptedException");
              return;
            } catch (Exception e) {
              log.error("Recv is down: ", e);
              failed = true;
            } finally {
              if (wts != null) {
                wts.cleanup();
              }
            }
          }
        };
    one.start();
    BlockingQueue<Invite> invites = new LinkedBlockingQueue<>();
    BlockingQueue<String> messages = new LinkedBlockingQueue<>();
    BlockingQueue<Move> moves = new LinkedBlockingQueue<>();
    LanServerController wts = null;
    try {
      wts =
          new LanServerController(
              invites, messages, moves, localAddr, 10001, 10000, "eHh4", "eHh4", new HashMap<>());
      TimeUnit.SECONDS.sleep(3);
      log.info("Got invite code: {}", wts.getInviteCode().get());
      log.info("Got active players: {}", wts.getActivePlayers().get());
      while (true) {
        if (inviteCode!= "") {
          break;
        }
      }
      wts.invite(inviteCode).get();
      wts.move(new Move(4, 3, 2, 1));
      wts.chatSendMsg("t1");
      wts.chatSendMsg("t2");
      wts.chatSendMsg("t3");
      TimeUnit.SECONDS.sleep(2);
      one.interrupt();
    } catch (Exception e) {
      fail("Send is dead", e);
    } finally {
      if (wts != null) {
        wts.cleanup();
      }
    }
    try {
      one.join();
    } catch (InterruptedException e) {
      ;
    }
    if (failed) {
      fail("Recv is dead");
    }
    assertEquals(1, messages.size());
    assertEquals("tx1", new String(Base64.getDecoder().decode(messages.poll())));
    assertEquals(new Move(1, 2, 3, 4), moves.poll());
    //
    assertEquals(3, messagesThread.size());
    assertEquals("t1", new String(Base64.getDecoder().decode(messagesThread.poll())));
    assertEquals("t2", new String(Base64.getDecoder().decode(messagesThread.poll())));
    assertEquals("t3", new String(Base64.getDecoder().decode(messagesThread.poll())));
    assertEquals(new Move(4, 3, 2, 1), movesThread.poll());
  }
}
