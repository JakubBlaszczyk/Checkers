package com.pk.server;

import java.io.IOException;
import java.nio.channels.ClosedChannelException;
import java.security.InvalidAlgorithmParameterException;
import java.util.List;

import com.pk.server.exceptions.ChatMsgRejected;
import com.pk.server.exceptions.MoveRejected;
import com.pk.server.models.Invite;
import com.pk.server.models.Move;
import com.pk.server.models.Player;

public interface ServerController {
  public void move(Move move) throws ClosedChannelException, IOException, MoveRejected;
  public void chatSendMsg(String msg) throws ClosedChannelException, IOException, ChatMsgRejected ;
  public List<Player> findPlayers();
  public GameSession invite(Invite invite) throws InvalidAlgorithmParameterException, IOException;
  public GameSession acceptInvitation(Invite invite) throws IOException, InvalidAlgorithmParameterException;
}
