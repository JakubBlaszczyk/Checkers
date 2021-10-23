package com.pk.server;

import java.io.IOException;
import java.util.List;

import com.pk.server.exceptions.InvitationRejected;
import com.pk.server.exceptions.MoveRejected;
import com.pk.server.models.Invite;
import com.pk.server.models.Move;
import com.pk.server.models.Player;

public class WebServerController implements ServerController {

  @Override
  public void move(Move move) throws IOException, MoveRejected {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void chatSendMsg(String msg) throws IOException {
    // TODO Auto-generated method stub
    
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
