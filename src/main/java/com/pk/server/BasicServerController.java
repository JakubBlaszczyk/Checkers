package com.pk.server;

import java.io.IOException;
import java.net.DatagramSocket;
import java.nio.channels.ClosedChannelException;
import java.security.InvalidAlgorithmParameterException;
import java.util.List;
import java.util.concurrent.BlockingQueue;

import com.pk.server.models.Invite;
import com.pk.server.models.Move;
import com.pk.server.models.Player;

public class BasicServerController implements ServerController {
  
  private GameSession gameSession = null;
  private ProbeResponder pResponder;
  private UdpServer udpServer;
  private TcpServer tcpServer;
  private Thread tcpThread;
  private Thread pResponderThread;

  public BasicServerController(BlockingQueue<Invite> bQueue, String ip, Integer port, String nick, String profileImg) throws IOException {
    udpServer = new BasicUdpServer(new DatagramSocket(port));
    pResponder = new BasicProbeResponder(nick, profileImg);
    pResponderThread = new Thread(pResponder);
    tcpServer = new BasicTcpServer(bQueue, ip, port);
    tcpThread = new Thread(tcpServer);
    
    tcpThread.start();
    pResponderThread.start();
  }

  @Override
  public void move(Move move) throws ClosedChannelException, IOException {
    if (gameSession == null) {
      throw new ClosedChannelException();
    }
    gameSession.move(move);
  }

  @Override
  public void chatSendMsg(String msg) throws ClosedChannelException, IOException {
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
