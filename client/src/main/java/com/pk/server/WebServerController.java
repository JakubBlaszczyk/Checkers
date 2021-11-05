package com.pk.server;

import com.pk.server.exceptions.InvitationRejected;
import com.pk.server.exceptions.MoveRejected;
import com.pk.server.models.Move;
import com.pk.server.models.Player;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.Future;
import lombok.AllArgsConstructor;

@AllArgsConstructor
public class WebServerController implements ServerController {
  WebTcpClient wTcpClient;

  @Override
  public void move(Move move) throws IOException, MoveRejected {
    wTcpClient.move(move);
  }

  @Override
  public void chatSendMsg(String msg) throws IOException {
    wTcpClient.chatSendMsg(msg);
  }

  @Override
  public Future<List<Player>> getActivePlayers() throws IOException {
    return wTcpClient.getActivePlayers();
  }

  @Override
  public Future<Boolean> invite(String inviteCode) throws InvitationRejected, IOException {
    return wTcpClient.invite(inviteCode);
  }

  @Override
  public boolean acceptInvitation(String inviteCode) throws IOException {
    return wTcpClient.acceptInvitation(inviteCode);
  }
}
