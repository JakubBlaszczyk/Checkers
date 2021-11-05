package com.pk.server;

import com.pk.server.exceptions.InvitationRejected;
import com.pk.server.exceptions.MoveRejected;
import com.pk.server.models.Invite;
import com.pk.server.models.Move;
import com.pk.server.models.Player;
import java.io.IOException;
import java.util.List;
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
  public List<Player> getActivePlayers() throws IOException {
    wTcpClient.getActivePlayers();
    return null;
  }

  @Override
  public boolean invite(String inviteCode) throws InvitationRejected, IOException {
    wTcpClient.invite(inviteCode);
    return false;
  }

  @Override
  public boolean acceptInvitation(Invite invite) throws IOException {
    // TODO Auto-generated method stub
    return false;
  }
}
