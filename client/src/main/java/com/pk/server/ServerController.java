package com.pk.server;

import com.pk.server.exceptions.InvitationRejected;
import com.pk.server.exceptions.MoveRejected;
import com.pk.server.models.Move;
import com.pk.server.models.Player;
import java.io.IOException;
import java.security.InvalidAlgorithmParameterException;
import java.util.List;
import java.util.concurrent.Future;

/** Main networking interface, containing all that is needed to pass data via network */
public interface ServerController {
  /**
   * Send move to player.
   *
   * @param move New move that needs to be send to second player.
   * @throws IOException thrown if underlying channel is closed.
   * @throws MoveRejected thrown if player responded in non standard way.
   */
  public void move(Move move) throws IOException, MoveRejected;

  /**
   * Send message to chat.
   *
   * @param msg Chat message to send
   * @throws IOException thrown if underlying channel is closed.
   */
  public void chatSendMsg(String msg) throws IOException;

  /**
   * Get all active players.
   *
   * @return Active players.
   */
  public Future<List<Player>> getActivePlayers() throws IOException;

  /**
   * Invite player to game.
   *
   * @param inviteCode remote side invitation code.
   * @return placeholder
   * @throws InvalidAlgorithmParameterException placeholder
   * @throws IOException thrown if underlying channel is closed.
   */
  public Future<Boolean> invite(String inviteCode) throws InvitationRejected, IOException;

  /**
   * @param invite player invitation code.
   * @return created session with player. If cannot create will return null.
   * @throws IOException thrown if underlying channel is closed.
   * @throws InvalidAlgorithmParameterException placeholder
   */
  public boolean acceptInvitation(String inviteCode) throws IOException;
}
