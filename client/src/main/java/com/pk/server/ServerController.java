package com.pk.server;

import java.io.IOException;
import java.security.InvalidAlgorithmParameterException;
import java.util.List;

import com.pk.server.exceptions.InvitationRejected;
import com.pk.server.exceptions.MoveRejected;
import com.pk.server.models.Invite;
import com.pk.server.models.Move;
import com.pk.server.models.Player;

/**
 * Class wrapping TCP and UDP servers.
 */
public interface ServerController {
  /**
   * Send move to player.
   * 
   * @param move New move that needs to be send to second player.
   * @throws IOException  thrown if underlying channel is closed.
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
   * Find active players in LAN.
   * 
   * @return Active players in LAN.
   */
  public List<Player> findPlayers();

  /**
   * Invite player to game.
   * 
   * @param player player to invite
   * @return created session with player. If cannot create will return null.
   * @throws InvalidAlgorithmParameterException placeholder
   * @throws IOException                        thrown if underlying channel is
   *                                            closed.
   */
  public boolean invite(Player player) throws InvitationRejected, IOException;

  /**
   * 
   * @param invite players invitation.
   * @return created session with player. If cannot create will return null.
   * @throws IOException                        thrown if underlying channel is
   *                                            closed.
   * @throws InvalidAlgorithmParameterException placeholder
   */
  public boolean acceptInvitation(Invite invite) throws IOException;
}
