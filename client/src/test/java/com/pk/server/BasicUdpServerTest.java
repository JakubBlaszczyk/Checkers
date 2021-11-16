// package com.pk.server;

// import static org.junit.jupiter.api.Assertions.assertEquals;
// import static org.mockito.ArgumentMatchers.any;
// import static org.mockito.Mockito.doAnswer;
// import static org.mockito.Mockito.doNothing;
// import static org.mockito.Mockito.mock;

// import com.pk.lanserver.BasicUdpServer;
// import com.pk.lanserver.models.Player;

// import java.net.DatagramPacket;
// import java.net.DatagramSocket;
// import java.net.InetAddress;
// import java.net.SocketException;
// import java.net.SocketTimeoutException;
// import java.util.List;
// import java.util.concurrent.Future;
// import java.util.concurrent.TimeUnit;

// import lombok.extern.slf4j.Slf4j;
// import org.junit.jupiter.api.BeforeEach;
// import org.junit.jupiter.api.Test;
// import org.mockito.ArgumentCaptor;
// import org.mockito.invocation.InvocationOnMock;
// import org.mockito.stubbing.Answer;

// @Slf4j
// public class BasicUdpServerTest {
//   DatagramSocket socket;
//   BasicUdpServer bUdpServer;

//   @BeforeEach
//   public void init() throws SocketException {
//     socket = mock(DatagramSocket.class);
//     bUdpServer =
//         new BasicUdpServer("test", "test", 10000) {
//           protected DatagramSocket createSocket(Integer port) throws SocketException {
//             return socket;
//           }
//           ;
//         };
//   }

//   @Test
//   public void testIfCorrectProbeBeingSend() throws Exception {
//     ArgumentCaptor<DatagramPacket> packetCaptor = ArgumentCaptor.forClass(DatagramPacket.class);
//     doNothing()
//         .doThrow(new RuntimeException("send called second time"))
//         .when(socket)
//         .send(packetCaptor.capture());

//     doAnswer(
//             new Answer<Void>() {
//               public Void answer(InvocationOnMock invocation) throws SocketTimeoutException {
//                 throw new SocketTimeoutException();
//               }
//             })
//         .when(socket)
//         .receive(any(DatagramPacket.class));

//     // bUdpServer.findPlayers();
//     bUdpServer.getActivePlayers().isDone();
//     DatagramPacket dp = packetCaptor.getValue();

//     assertEquals(InetAddress.getByName("255.255.255.255"), dp.getAddress());
//     assertEquals(10000, dp.getPort());
//     assertEquals("checkers:probe", new String(dp.getData()));
//   }

//   @Test
//   public void testParsingCorrectResp() throws Exception {
//     byte[] bytes = "checkers:probeResp testNik dGVzdA==".getBytes();
//     DatagramPacket receivedPacket =
//         new DatagramPacket(bytes, bytes.length, InetAddress.getByName("192.168.0.105"), 12345);
//     doNothing()
//         .doThrow(new RuntimeException("send called second time"))
//         .when(socket)
//         .send(any(DatagramPacket.class));

//     doAnswer(
//             new Answer<Void>() {
//               public Void answer(InvocationOnMock invocation) {
//                 Object[] args = invocation.getArguments();
//                 DatagramPacket dp = (DatagramPacket) args[0];
//                 dp.setData(receivedPacket.getData(), 0, receivedPacket.getLength());
//                 dp.setLength(receivedPacket.getLength());
//                 dp.setAddress(receivedPacket.getAddress());
//                 dp.setPort(receivedPacket.getPort());
//                 return null;
//               }
//             })
//         .doAnswer(
//             new Answer<Void>() {
//               public Void answer(InvocationOnMock invocation) throws SocketTimeoutException, InterruptedException {
//                 // TimeUnit.SECONDS.sleep(5);
//                 throw new SocketTimeoutException();
//               }
//             })
//         .when(socket)
//         .receive(any(DatagramPacket.class));

//     // List<Player> players = bUdpServer.findPlayers();
//     Future<List<Player>> futPlayers = bUdpServer.getActivePlayers();
//     List<Player> players = futPlayers.get();

//     assertEquals(1, players.size());
//     Player player = players.get(0);
//     assertEquals(InetAddress.getByName("192.168.0.105"), player.getIp());
//     assertEquals("testNik", player.getNick());
//     assertEquals("dGVzdA==", player.getProfileImg());
//   }

//   // @ParameterizedTest
//   // @ValueSource(strings = { "checkers:probeResptestNik dGVzdA==", "checkers :probeResptestNik
//   // dGVzdA==",
//   //     "checkers:probeResptestNik dGVz dA==", "checkers:pro beResptestNik dGVzdA==",
//   //     "checkers:probeResp  testNik dGVzdA==" })
//   // public void testParsingInvalidResp(String str) throws Exception {
//   //   byte[] bytes = str.getBytes();
//   //   DatagramPacket receivedPacket = new DatagramPacket(bytes, bytes.length,
//   // InetAddress.getByName("192.168.0.105"),
//   //       12345);

//   //   doNothing().doThrow(new RuntimeException("send called second
//   // time")).when(socket).send(any(DatagramPacket.class));

//   //   doAnswer(new Answer<Void>() {
//   //     public Void answer(InvocationOnMock invocation) {
//   //       Object[] args = invocation.getArguments();
//   //       DatagramPacket dp = (DatagramPacket) args[0];
//   //       dp.setData(receivedPacket.getData(), 0, receivedPacket.getLength());
//   //       dp.setLength(receivedPacket.getLength());
//   //       dp.setAddress(receivedPacket.getAddress());
//   //       dp.setPort(receivedPacket.getPort());
//   //       return null;
//   //     }
//   //   }).doAnswer(new Answer<Void>() {
//   //     public Void answer(InvocationOnMock invocation) throws SocketTimeoutException {
//   //       throw new SocketTimeoutException();
//   //     }
//   //   }).when(socket).receive(any(DatagramPacket.class));

//   //   List<Player> players = bUdpServer.findPlayers();
//   //   assertEquals(0, players.size());
//   // }
// }
