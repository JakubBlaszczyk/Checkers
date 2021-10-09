package com.pk.server;

import java.util.List;

import com.pk.server.models.Player;

public interface UdpServer {
    public static final int TIMEOUT = 5000;
    public static final int BUFFER_SIZE = 100;
    public static final int APP_PORT = 10000;

    public List<Player> findPlayers();
}
