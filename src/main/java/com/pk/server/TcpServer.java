package com.pk.server;

import java.io.IOException;
import java.security.InvalidAlgorithmParameterException;
import java.util.concurrent.Callable;

import com.pk.server.models.Invite;

public interface TcpServer extends Callable<Integer> {
  public GameSession invite(Invite invite) throws InvalidAlgorithmParameterException, IOException ;
  public GameSession acceptInvitation(Invite invite) throws InvalidAlgorithmParameterException, IOException;
  public void cleanup() throws IOException;
}
