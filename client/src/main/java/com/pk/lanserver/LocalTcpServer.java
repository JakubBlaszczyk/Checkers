package com.pk.lanserver;

import java.io.IOException;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;

import com.pk.lanserver.exceptions.InvitationRejected;
import com.pk.lanserver.exceptions.MoveRejected;
import com.pk.lanserver.models.Move;

/** Class used to perform all actions on Tcp stream. */
public interface LocalTcpServer extends Callable<Integer> {
  /**
   * Method used to create GameSession with specified player.
   *
   * @param player selected players invite
   * @return new instance of GameSession
   */
  public Future<Boolean> invite(String inviteCode) throws InvitationRejected, IOException;

  /**
   * Method used to create GameSession to specified player.
   *
   * @param invite selected players invitation.
   * @return new instance of GameSession.
   */
  public boolean acceptInvitation(String inviteCode) throws IOException;

  /**
   * Method used to close socket and selector used by Tcp server.
   *
   * @throws IOException if socket or selector is already closed.
   */
  public void cleanup() throws IOException;
  //////////////
  /**
   * Send move to player.
   *
   * @param move move to send.
   * @throws IOException thrown if underlying channel is closed.
   * @throws MoveRejected thrown if player responded in non accepted way.
   */
  public void move(Move move) throws IOException, MoveRejected;

  /**
   * Send message to player.
   *
   * @param msg base64 encoded message to send.
   * @throws IOException thrown if underlying channel is closed.
   */
  public void chatSendMsg(String msg) throws IOException;

  public void setNick(String nick);

  public void setProfileImg(String profileImg);

  public Future<String> getInviteCode();
}
