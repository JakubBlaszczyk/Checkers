package com.pk.lanserver;

import com.pk.lanserver.models.Player;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;

/** Class used to perform all actions on Udp side. */
public interface UdpServer extends Callable<Integer> {
  /** Socket timeout interval. */
  public static final int TIMEOUT = 5000;
  /** Socket input/output buffer size. */
  public static final int BUFFER_SIZE = 100;
  /** Servers port. */
  public static final int APP_PORT = 10000;

  /**
   * Sends probe packet to the broadcast and waits for the response.
   *
   * @return list of all players found in LAN
   */
  public Future<List<Player>> getActivePlayers();

  public void setNick(String nick);

  public void setProfileImg(String profileImg);

  public void cleanup();
}
