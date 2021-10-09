package com.pk.server;

import java.io.IOException;
import java.security.InvalidAlgorithmParameterException;
import java.util.List;

import com.pk.server.models.Invite;
import com.pk.server.models.Move;
import com.pk.server.models.Player;

public interface ServerController {
  public void move(Move move) throws IOException;
  public void chatSendMsg(String msg) throws IOException;
  public List<Player> findPlayers();
  public GameSession invite(Invite invite) throws InvalidAlgorithmParameterException, IOException;
  public GameSession acceptInvitation(Invite invite) throws IOException, InvalidAlgorithmParameterException;
}
