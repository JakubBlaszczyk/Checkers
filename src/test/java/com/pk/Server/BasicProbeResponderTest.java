package com.pk.Server;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;

import java.io.IOException;
import java.lang.reflect.Method;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.util.Arrays;

import com.pk.server.BasicProbeResponder;
import com.pk.server.ProbeResponder;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.Mockito;

/**
 * Unit tests for thread responding to probe beacons.
 */
public class BasicProbeResponderTest {
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
    byte[] buf = "checkers:probe".getBytes();
    DatagramPacket dp = new DatagramPacket(buf, buf.length);

    modifyAnswer(buf);

    mockedSocket.receive(dp);
    System.out.println("Data: " + new String(dp.getData()));

    String msg = (String) recvMsgMethod.invoke(pResponder, mockedSocket);
    assertEquals("checkers:probe", msg);

    dp = (DatagramPacket) prepareResponseMethod.invoke(pResponder, msg);
    String validResponse = String.format("checkers:probeResp %s %s", pResponder.getNick(), pResponder.getProfileImg());
    assertTrue(Arrays.equals(validResponse.getBytes(), dp.getData()));
    assertEquals(10000, dp.getPort());
    assertEquals(InetAddress.getByName("255.255.255.255"), dp.getAddress());
    assertEquals(validResponse.length(), dp.getLength());
  }


  @ParameterizedTest
  @ValueSource(strings = {"check", "checkers:pro", "checkers:probe   sad as sad asd as asd", "checkers:probes"})
  void testInvalidBeacons(String str) throws Exception {
    byte[] buf = str.getBytes();
    DatagramPacket dp = new DatagramPacket(buf, buf.length);

    modifyAnswer(buf);

    mockedSocket.receive(dp);
    System.out.println("Data: " + new String(dp.getData()));

    String msg = (String) recvMsgMethod.invoke(pResponder, mockedSocket);
    assertEquals(str, msg);

    dp = (DatagramPacket) prepareResponseMethod.invoke(pResponder, msg);
    assertEquals(null, dp);
  }

  private void modifyAnswer(byte[] buf) throws IOException {
    InetAddress hostAddress = InetAddress.getByName("255.255.255.255");
    doAnswer(invocation -> {
      Object[] args = invocation.getArguments();
      ((DatagramPacket) args[0]).setLength(buf.length);
      ((DatagramPacket) args[0]).setData(buf);
      ((DatagramPacket) args[0]).setAddress(hostAddress);
      ((DatagramPacket) args[0]).setPort(10000);
      return null;
    }).when(mockedSocket).receive(any(DatagramPacket.class));
  }
}
