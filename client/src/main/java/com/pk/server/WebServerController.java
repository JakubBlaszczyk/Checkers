package com.pk.server;

import java.io.IOException;
import java.util.List;

import com.pk.server.exceptions.InvitationRejected;
import com.pk.server.exceptions.MoveRejected;
import com.pk.server.models.Invite;
import com.pk.server.models.Move;
import com.pk.server.models.Player;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

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
  public List<Player> findPlayers() {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public boolean invite(Player player) throws InvitationRejected, IOException {
    // TODO Auto-generated method stub
    return false;
  }

  @Override
  public boolean acceptInvitation(Invite invite) throws IOException {
    // TODO Auto-generated method stub
    return false;
  }
  
}
