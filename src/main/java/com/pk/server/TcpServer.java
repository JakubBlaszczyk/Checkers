package com.pk.server;

import java.io.IOException;
import java.security.InvalidAlgorithmParameterException;
import java.util.concurrent.Callable;

import com.pk.server.models.Invite;
import com.pk.server.models.Player;

/**
 * Class used to perform all actions on Tcp stream.
 */
public interface TcpServer extends Callable<Integer> {
  /**
   * Method used to create GameSession with specified player.
   * 
   * @param player selected players invite
   * @return new instance of GameSession
   */
  public GameSession invite(Player invite) throws InvalidAlgorithmParameterException, IOException ;
  /**
   * Method used to create GameSession to specified player.
   * 
   * @param invite selected players invitation.
   * @return new instance of GameSession.
   */
  public GameSession acceptInvitation(Invite invite) throws InvalidAlgorithmParameterException, IOException;
  /**
   * Method used to close socket and selector used by Tcp server.
   * 
   * @throws IOException if socket or selector is already closed.
   */
  public void cleanup() throws IOException;
}
