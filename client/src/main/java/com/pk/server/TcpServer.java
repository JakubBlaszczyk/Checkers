package com.pk.server;

import java.io.IOException;
import java.net.Socket;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;

import com.pk.server.exceptions.InvitationRejected;
import com.pk.server.exceptions.MoveRejected;
import com.pk.server.models.Invite;
import com.pk.server.models.Move;
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
  public boolean invite(Player invite) throws InvitationRejected, IOException;

  /**
   * Method used to create GameSession to specified player.
   * 
   * @param invite selected players invitation.
   * @return new instance of GameSession.
   */
  public boolean acceptInvitation(Invite invite) throws IOException;

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
   * @throws IOException  thrown if underlying channel is closed.
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

  public BlockingQueue<String> getBQueueMsgs();

  public BlockingQueue<Move> getBQueueMoves();

  public Socket getSocket();
}
