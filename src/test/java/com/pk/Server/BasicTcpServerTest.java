// package com.pk.Server;

// import static org.junit.Assert.assertEquals;
// import static org.junit.Assert.assertNotNull;
// import static org.junit.Assert.assertNull;
// import static org.junit.Assert.fail;
// import static org.junit.jupiter.api.Assertions.assertTrue;
// import static org.mockito.ArgumentMatchers.any;
// import static org.mockito.Mockito.doAnswer;
// import static org.mockito.Mockito.when;

// import java.io.IOException;
// import java.net.InetAddress;
// import java.net.Socket;
// import java.nio.ByteBuffer;
// import java.nio.channels.SocketChannel;
// import java.util.Arrays;
// import java.util.concurrent.BlockingQueue;
// import java.util.concurrent.ExecutorService;
// import java.util.concurrent.Executors;
// import java.util.concurrent.Future;
// import java.util.concurrent.LinkedBlockingQueue;

// import com.pk.server.BasicTcpServer;
// import com.pk.server.GameSession;
// import com.pk.server.TcpServer;
// import com.pk.server.models.Invite;

// import org.junit.Ignore;
// import org.junit.jupiter.api.Test;
// import org.junit.runner.RunWith;
// import org.mockito.ArgumentCaptor;
// import org.mockito.invocation.InvocationOnMock;
// import org.mockito.stubbing.Answer;
// import org.powermock.api.mockito.PowerMockito;
// import org.powermock.core.classloader.annotations.PrepareForTest;
// import org.powermock.modules.junit4.PowerMockRunner;


// @RunWith(PowerMockRunner.class)
// @PrepareForTest({ BasicTcpServer.class, SocketChannel.class })
// public class BasicTcpServerTest {
//   @Test
//   public void testIfServerIsAcceptingConnections() throws Exception {
//     BlockingQueue<Invite> bQueue = new LinkedBlockingQueue<>();
//     TcpServer tcpServer = new BasicTcpServer(bQueue, "127.0.0.1", 10000);
//     // Thread th = new Thread(tcpServer);
//     ExecutorService executorService = Executors.newFixedThreadPool(1);
//     Future<Integer> futureTcp = executorService.submit(tcpServer);
//     Socket[] socks = new Socket[10];
//     for (int i = 0; i < 10; ++i) {
//       socks[i] = new Socket(InetAddress.getByName("127.0.0.1"), 10000);
//       assertTrue(socks[i].isConnected());
//     }
//     futureTcp.cancel(true);
//     tcpServer.cleanup();
//   }

//   @Test
//   public void testInviteAccepted() throws IOException {
//     BlockingQueue<Invite> bQueue = new LinkedBlockingQueue<>();
//     BasicTcpServer tcpServer = new BasicTcpServer(bQueue, "127.0.0.1", 10000);
//     SocketChannel sc = PowerMockito.mock(SocketChannel.class);

//     PowerMockito.when(sc.isBlocking()).thenReturn(true);
//     PowerMockito.when(sc.isConnected()).thenReturn(true);
//     PowerMockito.when(sc.isOpen()).thenReturn(true);

//     ArgumentCaptor<ByteBuffer> bytesCaptor = ArgumentCaptor.forClass(ByteBuffer.class);
//     when(sc.write(bytesCaptor.capture())).thenReturn(10);

//     doAnswer(new Answer<Integer>() {
//       public Integer answer(InvocationOnMock invocation) {
//         Object[] args = invocation.getArguments();
//         ByteBuffer bb = (ByteBuffer) args[0];
//         byte[] tmp = bb.array();
//         byte[] x = "checkers:invitationOk".getBytes();
//         for (int i = 0; i < "checkers:invitationOk".length(); ++i) {
//           tmp[i] = x[i];
//         }
//         return 10;
//       }
//     }).when(sc).read(any(ByteBuffer.class));

//     GameSession gs = null;
//     try {
//       // gs = tcpServer.invite(new Invite(InetAddress.getByName("127.0.0.1"), "nik", "dGVzdA==", sc));
//     } catch (Exception e) {
//       fail("Exception cannot happen");
//     }
//     assertTrue(Arrays.equals("checkers:invitationAsk".getBytes(), bytesCaptor.getValue().array()));
//     assertNotNull(gs);
//     assertEquals(0, bQueue.size());

//     tcpServer.cleanup();
//   }

//   @Test
//   public void testInviteRejected() throws Exception {
//     BlockingQueue<Invite> bQueue = new LinkedBlockingQueue<>();
//     BasicTcpServer tcpServer = new BasicTcpServer(bQueue, "127.0.0.1", 10000);
//     SocketChannel sc = PowerMockito.mock(SocketChannel.class);

//     PowerMockito.when(sc.isBlocking()).thenReturn(true);
//     PowerMockito.when(sc.isConnected()).thenReturn(true);
//     PowerMockito.when(sc.isOpen()).thenReturn(true);

//     ArgumentCaptor<ByteBuffer> bytesCaptor = ArgumentCaptor.forClass(ByteBuffer.class);
//     ;
//     when(sc.write(bytesCaptor.capture())).thenReturn(10);

//     doAnswer(new Answer<Integer>() {
//       public Integer answer(InvocationOnMock invocation) {
//         Object[] args = invocation.getArguments();
//         ByteBuffer bb = (ByteBuffer) args[0];
//         byte[] tmp = bb.array();
//         byte[] x = "checkers:invitationRejected".getBytes();
//         for (int i = 0; i < "checkers:invitationRejected".length(); ++i) {
//           tmp[i] = x[i];
//         }
//         return 10;
//       }
//     }).when(sc).read(any(ByteBuffer.class));

//     GameSession gs = null;
//     try {
//       // gs = tcpServer.invite(new Invite(InetAddress.getByName("127.0.0.1"), "nik", "dGVzdA==", sc));
//       fail("Exception cannot happen");
//     } catch (Exception e) {
//     }
//     assertTrue(Arrays.equals("checkers:invitationAsk".getBytes(), bytesCaptor.getValue().array()));
//     assertNull(gs);
//     assertEquals(0, bQueue.size());

//     tcpServer.cleanup();
//   }

//   @Test
//   public void testRandomResp() throws Exception {
//     BlockingQueue<Invite> bQueue = new LinkedBlockingQueue<>();
//     BasicTcpServer tcpServer = new BasicTcpServer(bQueue, "127.0.0.1", 10000);
//     SocketChannel sc = PowerMockito.mock(SocketChannel.class);

//     PowerMockito.when(sc.isBlocking()).thenReturn(true);
//     PowerMockito.when(sc.isConnected()).thenReturn(true);
//     PowerMockito.when(sc.isOpen()).thenReturn(true);

//     ArgumentCaptor<ByteBuffer> bytesCaptor = ArgumentCaptor.forClass(ByteBuffer.class);
//     ;
//     when(sc.write(bytesCaptor.capture())).thenReturn(10);

//     doAnswer(new Answer<Integer>() {
//       public Integer answer(InvocationOnMock invocation) {
//         Object[] args = invocation.getArguments();
//         ByteBuffer bb = (ByteBuffer) args[0];
//         byte[] tmp = bb.array();
//         byte[] x = "checkers:invisdlksadkl kl sakl sakl sadlk".getBytes();
//         for (int i = 0; i < "checkers:invisdlksadkl kl sakl sakl sadlk".length(); ++i) {
//           tmp[i] = x[i];
//         }
//         return 10;
//       }
//     }).when(sc).read(any(ByteBuffer.class));

//     GameSession gs = null;
//     try {
//       // gs = tcpServer.invite(new Invite(InetAddress.getByName("127.0.0.1"), "nik", "dGVzdA==", sc));
//       fail("Exception cannot happen");
//     } catch (Exception e) {
//     }
//     assertTrue(Arrays.equals("checkers:invitationAsk".getBytes(), bytesCaptor.getValue().array()));
//     assertNull(gs);
//     assertEquals(0, bQueue.size());

//     tcpServer.cleanup();
//   }
// }
