package com.pk.server;

import java.io.IOException;
import java.net.DatagramSocket;
import java.nio.channels.ClosedChannelException;
import java.security.InvalidAlgorithmParameterException;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import com.pk.server.exceptions.ChatMsgRejected;
import com.pk.server.exceptions.MoveRejected;
import com.pk.server.models.Invite;
import com.pk.server.models.Move;
import com.pk.server.models.Player;

import lombok.Getter;

public class BasicServerController implements ServerController {
  
  private GameSession gameSession = null;
  private ProbeResponder pResponder;
  private UdpServer udpServer;
  private TcpServer tcpServer;
  // FIXME threads num shouldn't be magic number
  private ExecutorService executorService = Executors.newFixedThreadPool(1);

  private @Getter Future<Integer> futureResponder;
  private @Getter Future<Integer> futureTcp;

  public BasicServerController(BlockingQueue<Invite> bQueue, String ip, Integer port, String nick, String profileImg) throws IOException {
    udpServer = new BasicUdpServer(new DatagramSocket(port));
    tcpServer = new BasicTcpServer(bQueue, ip, port);
    pResponder = new BasicProbeResponder(nick, profileImg);
    futureResponder = executorService.submit(pResponder);
    futureTcp = executorService.submit(tcpServer);
  }

  @Override
  public void move(Move move) throws ClosedChannelException, IOException, MoveRejected {
    if (gameSession == null) {
      throw new ClosedChannelException();
    }
    gameSession.move(move);
  }

  @Override
  public void chatSendMsg(String msg) throws ClosedChannelException, IOException, ChatMsgRejected {
    if (gameSession == null) {
      throw new ClosedChannelException();
    }
    gameSession.chatSendMsg(msg);
  }

  @Override
  public List<Player> findPlayers() {
    return udpServer.findPlayers();
  }

  @Override
  public GameSession invite(Invite invite) throws InvalidAlgorithmParameterException, IOException {
    return tcpServer.invite(invite);
  }

  @Override
  public GameSession acceptInvitation(Invite invite) throws IOException, InvalidAlgorithmParameterException {
    return tcpServer.acceptInvitation(invite);
  }
  
}
