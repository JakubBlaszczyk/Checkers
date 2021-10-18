package com.pk.server;

import java.util.List;

import com.pk.server.models.Player;

/**
 * Class used to perform all actions on Udp side.
 */
public interface UdpServer {
    /**
     * Socket timeout interval.
     */
    public static final int TIMEOUT = 5000;
    /**
     * Socket input/output buffer size.
     */
    public static final int BUFFER_SIZE = 100;
    /**
     * Servers port.
     */
    public static final int APP_PORT = 10000;

    /**
     * Sends probe packet to the broadcast and waits for the response.
     * 
     * @return list of all players found in LAN
     */
    public List<Player> findPlayers();
}
