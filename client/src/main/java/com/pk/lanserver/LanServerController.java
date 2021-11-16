package com.pk.lanserver;

import com.pk.lanserver.exceptions.InvitationRejected;
import com.pk.lanserver.exceptions.MoveRejected;
import com.pk.lanserver.models.Invite;
import com.pk.lanserver.models.Move;
import com.pk.lanserver.models.Player;
import java.io.IOException;
import java.net.Socket;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import lombok.Getter;

/** */
public class LanServerController implements ServerController {
  private UdpServer udpServer;
  private LocalTcpServer tcpServer;
  private ExecutorService executorService = Executors.newFixedThreadPool(2);
  private @Getter Future<Integer> futureUdp;
  private @Getter Future<Integer> futureTcp;

  /**
   * @param bQueue queue which will contain all received invites
   * @param ip ip to bind
   * @param port port to bind
   * @param nick current player nickname
   * @param profileImg current player profileImg b64 encoded
   * @throws IOException if underling channel is closed.
   */
  public LanServerController(
      BlockingQueue<Invite> bQI,
      BlockingQueue<String> bQS,
      BlockingQueue<Move> bQM,
      String localIp,
      Integer port,
      String nick,
      String profileImg,
      Map<String, Socket> mapInvToSock)
      throws IOException {
    udpServer = new BasicUdpServer(nick, profileImg, port);
    tcpServer = new LanTcpServer(bQI, bQS, bQM, localIp, port, "dDI=", "dDI=", new HashMap<>());
    futureUdp = executorService.submit(udpServer);
    futureTcp = executorService.submit(tcpServer);
  }

  @Override
  public void move(Move move) throws IOException, MoveRejected {
    tcpServer.move(move);
  }

  @Override
  public void chatSendMsg(String msg) throws IOException {
    tcpServer.chatSendMsg(msg);
  }

  @Override
  public Future<List<Player>> getActivePlayers() throws IOException {
    return udpServer.getActivePlayers();
  }

  @Override
  public Future<Boolean> invite(String inviteCode) throws InvitationRejected, IOException {
    return tcpServer.invite(inviteCode);
  }

  @Override
  public boolean acceptInvitation(String inviteCode) throws IOException {
    return tcpServer.acceptInvitation(inviteCode);
  }

  @Override
  public Future<String> getInviteCode() {
    return tcpServer.getInviteCode();
  }
}
