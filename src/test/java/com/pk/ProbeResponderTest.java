package com.pk;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;

import java.lang.reflect.Method;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.util.Arrays;

import com.pk.server.BasicProbeResponder;
import com.pk.server.ProbeResponder;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

/**
 * Unit tests for thread responding to probe beacons.
 */
public class ProbeResponderTest {
  private Method recvMsgMethod;
  private Method prepareResponseMethod;
  private ProbeResponder pResponder;
  private DatagramSocket mockedSocket;

  @BeforeEach
  void init() throws Exception {
    pResponder = new BasicProbeResponder("nik", "dGVzdFN0cmluZw==");
    mockedSocket = Mockito.mock(DatagramSocket.class);
    prepareResponseMethod = pResponder.getClass().getDeclaredMethod("prepareResponse", String.class);
    recvMsgMethod = pResponder.getClass().getDeclaredMethod("recvMsg", DatagramSocket.class);
    recvMsgMethod.setAccessible(true);
    prepareResponseMethod.setAccessible(true);
  }

  @Test
  void testCorrectBeacon() throws Exception {
    InetAddress hostAddress = InetAddress.getByName("255.255.255.255");
    byte[] buf = "checkers:probe".getBytes();
    DatagramPacket dp = new DatagramPacket(buf, buf.length);
    DatagramPacket receivedPacket = new DatagramPacket(new byte[200], 200);

    doAnswer(invocation -> {
      Object[] args = invocation.getArguments();
      ((DatagramPacket) args[0]).setLength(buf.length);
      ((DatagramPacket) args[0]).setData(buf);
      ((DatagramPacket) args[0]).setAddress(hostAddress);
      ((DatagramPacket) args[0]).setPort(10000);
      return null;
    }).when(mockedSocket).receive(any(DatagramPacket.class));

    mockedSocket.receive(receivedPacket);
    System.out.println("Data: " + new String(receivedPacket.getData()));

    String msg = (String) recvMsgMethod.invoke(pResponder, mockedSocket);
    assertEquals("checkers:probe", msg);

    dp = (DatagramPacket) prepareResponseMethod.invoke(pResponder, msg);
    String validResponse = String.format("checkers:probeResp %s %s", pResponder.getNick(), pResponder.getProfileImg());
    assertTrue(Arrays.equals(validResponse.getBytes(), dp.getData()));
    assertEquals(10000, dp.getPort());
    assertEquals(hostAddress, dp.getAddress());
    assertEquals(validResponse.length(), dp.getLength());
  }

  @Test
  void testIncompleteCommand() throws Exception {
    InetAddress hostAddress = InetAddress.getByName("255.255.255.255");
    byte[] buf = "check".getBytes();
    DatagramPacket dp = new DatagramPacket(buf, buf.length);
    DatagramPacket receivedPacket = new DatagramPacket(new byte[200], 200);

    doAnswer(invocation -> {
      Object[] args = invocation.getArguments();
      ((DatagramPacket) args[0]).setLength(buf.length);
      ((DatagramPacket) args[0]).setData(buf);
      ((DatagramPacket) args[0]).setAddress(hostAddress);
      ((DatagramPacket) args[0]).setPort(10000);
      return null;
    }).when(mockedSocket).receive(any(DatagramPacket.class));

    mockedSocket.receive(receivedPacket);
    System.out.println("Data: " + new String(receivedPacket.getData()));

    String msg = (String) recvMsgMethod.invoke(pResponder, mockedSocket);
    assertEquals("check", msg);

    dp = (DatagramPacket) prepareResponseMethod.invoke(pResponder, msg);
    assertEquals(null, dp);
  }

  @Test
  void testIncompleteCommand2() throws Exception {
    InetAddress hostAddress = InetAddress.getByName("255.255.255.255");
    byte[] buf = "checkers:pro".getBytes();
    DatagramPacket dp = new DatagramPacket(buf, buf.length);
    DatagramPacket receivedPacket = new DatagramPacket(new byte[200], 200);

    doAnswer(invocation -> {
      Object[] args = invocation.getArguments();
      ((DatagramPacket) args[0]).setLength(buf.length);
      ((DatagramPacket) args[0]).setData(buf);
      ((DatagramPacket) args[0]).setAddress(hostAddress);
      ((DatagramPacket) args[0]).setPort(10000);
      return null;
    }).when(mockedSocket).receive(any(DatagramPacket.class));

    mockedSocket.receive(receivedPacket);
    System.out.println("Data: " + new String(receivedPacket.getData()));

    String msg = (String) recvMsgMethod.invoke(pResponder, mockedSocket);
    assertEquals("checkers:pro", msg);

    dp = (DatagramPacket) prepareResponseMethod.invoke(pResponder, msg);
    assertEquals(null, dp);
  }

  @Test
  void testTooLongBeacon() throws Exception {
    InetAddress hostAddress = InetAddress.getByName("255.255.255.255");
    byte[] buf = "checkers:probe   sad as sad asd as asd".getBytes();
    DatagramPacket dp = new DatagramPacket(buf, buf.length);
    DatagramPacket receivedPacket = new DatagramPacket(new byte[200], 200);

    doAnswer(invocation -> {
      Object[] args = invocation.getArguments();
      ((DatagramPacket) args[0]).setLength(buf.length);
      ((DatagramPacket) args[0]).setData(buf);
      ((DatagramPacket) args[0]).setAddress(hostAddress);
      ((DatagramPacket) args[0]).setPort(10000);
      return null;
    }).when(mockedSocket).receive(any(DatagramPacket.class));

    mockedSocket.receive(receivedPacket);
    System.out.println("Data: " + new String(receivedPacket.getData()));

    String msg = (String) recvMsgMethod.invoke(pResponder, mockedSocket);
    assertEquals("checkers:probe   sad as sad asd as asd", msg);

    dp = (DatagramPacket) prepareResponseMethod.invoke(pResponder, msg);
    assertEquals(null, dp);
  }

  @Test
  void testTooLongBeacon2() throws Exception {
    InetAddress hostAddress = InetAddress.getByName("255.255.255.255");
    byte[] buf = "checkers:probes".getBytes();
    DatagramPacket dp = new DatagramPacket(buf, buf.length);
    DatagramPacket receivedPacket = new DatagramPacket(new byte[200], 200);

    doAnswer(invocation -> {
      Object[] args = invocation.getArguments();
      ((DatagramPacket) args[0]).setLength(buf.length);
      ((DatagramPacket) args[0]).setData(buf);
      ((DatagramPacket) args[0]).setAddress(hostAddress);
      ((DatagramPacket) args[0]).setPort(10000);
      return null;
    }).when(mockedSocket).receive(any(DatagramPacket.class));

    mockedSocket.receive(receivedPacket);
    System.out.println("Data: " + new String(receivedPacket.getData()));

    String msg = (String) recvMsgMethod.invoke(pResponder, mockedSocket);
    assertEquals("checkers:probes", msg);

    dp = (DatagramPacket) prepareResponseMethod.invoke(pResponder, msg);
    assertEquals(null, dp);
  }
}
