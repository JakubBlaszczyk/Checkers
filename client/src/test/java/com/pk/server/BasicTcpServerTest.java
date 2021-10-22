package com.pk.server;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.util.Arrays;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;

import com.pk.server.models.Invite;
import com.pk.server.models.Player;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.Mockito;
public class BasicTcpServerTest {
  @Test
  public void testIfServerIsAcceptingConnections() throws Exception {
    BlockingQueue<Invite> bQueue = new LinkedBlockingQueue<>();
    TcpServer tcpServer = new BasicTcpServer(bQueue, "127.0.0.1", 10000);
    ExecutorService executorService = Executors.newFixedThreadPool(1);
    Future<Integer> futureTcp = executorService.submit(tcpServer);
    Socket sock = new Socket(InetAddress.getByName("127.0.0.1"), 10000);
    assertTrue(sock.isConnected());
    futureTcp.cancel(true);
    tcpServer.cleanup();
  }

  @Test
  public void testInviteAccepted() throws Exception {
    BasicTcpServer tcpServerMock = Mockito.mock(BasicTcpServer.class);
    Socket sockMock = Mockito.mock(Socket.class);
    byte[] input = "checkers:invitationOk".getBytes();
    ByteArrayOutputStream myOutputStream = new ByteArrayOutputStream();
    InputStream myInputStream = new ByteArrayInputStream(input);

    Mockito.when(sockMock.isConnected()).thenReturn(true);
    Mockito.when(sockMock.getOutputStream()).thenReturn(myOutputStream);
    Mockito.when(sockMock.getInputStream()).thenReturn(myInputStream);
    String testNick = "test";
    String testProfileImg = "test2";

    Mockito.doCallRealMethod().when(tcpServerMock).setNick(anyString());
    Mockito.doCallRealMethod().when(tcpServerMock).setProfileImg(anyString());

    tcpServerMock.setNick(testNick);
    tcpServerMock.setProfileImg(testProfileImg);

    Mockito.doReturn(sockMock).when(tcpServerMock).openSocketToPlayer(any(InetAddress.class));
    Mockito.when(tcpServerMock.invite(any(Player.class))).thenCallRealMethod();

    GameSession gs = null;
    try {
      gs = tcpServerMock.invite(new Player(InetAddress.getByName("127.0.0.1"), "nik", "dGVzdA=="));
    } catch (Exception e) {
      System.out.println(e.getMessage());
      fail("Exception cannot happen");
    }
    assertTrue(Arrays.equals(("checkers:invitationAsk " + testNick + " " + testProfileImg).getBytes(),
        myOutputStream.toByteArray()));
    assertNotNull(gs);
    assertNotNull(gs.getSocket());
    assertNotNull(gs.getBQueueMsgs());
    assertNotNull(gs.getBQueueMoves());
  }

  @Test
  public void testInviteRejected() throws Exception {
    BasicTcpServer tcpServerMock = Mockito.mock(BasicTcpServer.class);
    Socket sockMock = Mockito.mock(Socket.class);
    byte[] input = "checkers:invitationRejected".getBytes();
    ByteArrayOutputStream myOutputStream = new ByteArrayOutputStream();
    InputStream myInputStream = new ByteArrayInputStream(input);

    Mockito.when(sockMock.isConnected()).thenReturn(true);
    Mockito.when(sockMock.getOutputStream()).thenReturn(myOutputStream);
    Mockito.when(sockMock.getInputStream()).thenReturn(myInputStream);
    String testNick = "test";
    String testProfileImg = "test2";

    Mockito.doCallRealMethod().when(tcpServerMock).setNick(anyString());
    Mockito.doCallRealMethod().when(tcpServerMock).setProfileImg(anyString());

    tcpServerMock.setNick(testNick);
    tcpServerMock.setProfileImg(testProfileImg);

    Mockito.doReturn(sockMock).when(tcpServerMock).openSocketToPlayer(any(InetAddress.class));
    Mockito.when(tcpServerMock.invite(any(Player.class))).thenCallRealMethod();

    GameSession gs = null;
    try {
      gs = tcpServerMock.invite(new Player(InetAddress.getByName("127.0.0.1"), "nik", "dGVzdA=="));
      fail("Exception have to happen");
    } catch (Exception e) {
    }
    assertTrue(Arrays.equals(("checkers:invitationAsk " + testNick + " " + testProfileImg).getBytes(),
        myOutputStream.toByteArray()));
    assertNull(gs);
  }

  @ParameterizedTest
  @ValueSource(strings = { "checkers:invitationRejectedA", "checkers:invitationRej", "checkers:invitation", "checkers:",
      "checkers", "checker", "これは素晴らしい景色ですね" })
  public void testRandomResp(String arg) throws Exception {
    BasicTcpServer tcpServerMock = Mockito.mock(BasicTcpServer.class);
    Socket sockMock = Mockito.mock(Socket.class);
    byte[] input = arg.getBytes();
    ByteArrayOutputStream myOutputStream = new ByteArrayOutputStream();
    InputStream myInputStream = new ByteArrayInputStream(input);

    Mockito.when(sockMock.isConnected()).thenReturn(true);
    Mockito.when(sockMock.getOutputStream()).thenReturn(myOutputStream);
    Mockito.when(sockMock.getInputStream()).thenReturn(myInputStream);
    String testNick = "test";
    String testProfileImg = "test2";

    Mockito.doCallRealMethod().when(tcpServerMock).setNick(anyString());
    Mockito.doCallRealMethod().when(tcpServerMock).setProfileImg(anyString());

    tcpServerMock.setNick(testNick);
    tcpServerMock.setProfileImg(testProfileImg);

    Mockito.doReturn(sockMock).when(tcpServerMock).openSocketToPlayer(any(InetAddress.class));
    Mockito.when(tcpServerMock.invite(any(Player.class))).thenCallRealMethod();

    GameSession gs = null;
    try {
      gs = tcpServerMock.invite(new Player(InetAddress.getByName("127.0.0.1"), "nik", "dGVzdA=="));
      fail("Exception have to happen");
    } catch (Exception e) {
    }
    assertTrue(Arrays.equals(("checkers:invitationAsk " + testNick + " " + testProfileImg).getBytes(),
        myOutputStream.toByteArray()));
    assertNull(gs);
  }
}
