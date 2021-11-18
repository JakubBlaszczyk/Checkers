package com.pk;

import com.pk.models.Config;
import com.pk.models.Player;
import com.pk.models.Session;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.security.InvalidAlgorithmParameterException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.concurrent.ThreadLocalRandom;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.collections4.BidiMap;

@Slf4j
public class Server implements Callable<Integer> {
  private Selector selector;
  private ServerSocketChannel serverSocketChannel;
  // Nickname <-> SocketChannel
  private BidiMap<String, SocketChannel> connectedPlayersMap;
  // invite code <-> SocketChannel
  private BidiMap<String, SocketChannel> inviteCodeMap;
  // All active players
  private Set<Player> hashPlayers;
  // Players in game
  private Set<SocketChannel> playersInGames;

  private static final Integer INVITE_CODE_LENGTH = 12;

  public Server(
      String ip,
      Integer port,
      BidiMap<String, SocketChannel> connectedPlayersMap,
      BidiMap<String, SocketChannel> inviteCodeMap,
      Set<Player> hashPlayers,
      Set<SocketChannel> playersInGames)
      throws IOException {
    selector = Selector.open();
    serverSocketChannel = ServerSocketChannel.open();
    serverSocketChannel.configureBlocking(false);
    serverSocketChannel.bind(new InetSocketAddress(ip, port));
    serverSocketChannel.register(selector, SelectionKey.OP_ACCEPT);
    this.connectedPlayersMap = connectedPlayersMap;
    this.inviteCodeMap = inviteCodeMap;
    this.hashPlayers = hashPlayers;
    this.playersInGames = playersInGames;
  }

  public void cleanup() throws IOException {
    serverSocketChannel.close();
    selector.close();
  }

  @Override
  public Integer call() throws Exception {
    SelectionKey key = null;
    while (true) {
      if (Thread.currentThread().isInterrupted()) {
        return 0;
      }
      if (selector.select() <= 0) continue;
      Iterator<SelectionKey> iterator = selector.selectedKeys().iterator();
      while (iterator.hasNext()) {
        key = iterator.next();
        iterator.remove();
        try {
          if (key.isAcceptable()) {
            handleAccept();
          }
          if (key.isReadable()) {
            handleReadMsg(key);
          }
        } catch (IOException e) {
          log.error("Accept error, terminating");
          key.cancel();
        }
      }
    }
  }

  private void handleReadMsg(SelectionKey key) throws IOException {
    SocketChannel sc = (SocketChannel) key.channel();
    ByteBuffer bb = ByteBuffer.allocate(1024);
    int len = sc.read(bb);
    if (len <= 0) {
      sc.close();
      log.info("Connection closed");
      if (key.attachment() != null) {
        Session session = (Session) key.attachment();
        SocketChannel remote = session.getRemote();
        // TODO add some recovery mechanism ?
        log.info("One side session closed, closing both sides");
        handleClosedConnection(key, (SocketChannel) key.channel());
        handleClosedConnection(remote.keyFor(selector), remote);
      } else {
        handleClosedConnection(key, sc);
      }
      return;
    }
    String msg = new String(bb.array(), 0, len).strip();
    if (msg.charAt(msg.length() - 1) == '!') {
      msg = msg.substring(0, msg.length() - 1);
    }
    log.info("Message received: <{}>, len: {}", msg, msg.length());
    if (key.attachment() != null) {
      log.info("Session msg");
      handleSessionMsg(msg, key);
      return;
    }
    try {
      if (msg.startsWith("checkers:config ")) {
        handleConfig(sc, msg);
      } else if (msg.equals("checkers:randomGame")) {
        handleRandomGame(key, sc);
      } else if (msg.equals("checkers:getPlayers")) {
        sc.write(ByteBuffer.wrap(transformPlayersListIntoBytes()));
      } else if (msg.startsWith("checkers:inviteAsk ")) {
        handleInviteAsk(sc, msg.substring(19));
      } else if (msg.startsWith("checkers:inviteRejected ")) {
        handleInviteRejected(msg.substring(24));
      } else if (msg.startsWith("checkers:inviteOk ")) {
        handleInviteOk(sc, msg.substring(18));
      } else {
        log.warn("Got invalid message");
      }
    } catch (IOException e) {
      log.warn("Connection error, closing");
      handleClosedConnection(key, sc);
    }
  }

  private void handleSessionMsg(String msg, SelectionKey key) {
    Session session = (Session) key.attachment();
    SocketChannel remote = session.getRemote();
    try {
      remote.write(ByteBuffer.wrap(msg.getBytes()));
    } catch (IOException e) {
      // TODO add some recovery mechanism ?
      log.error("Exception in handleSessionMsg, closing both sides: ", e);
      handleClosedConnection(key, (SocketChannel) key.channel());
      handleClosedConnection(remote.keyFor(selector), remote);
    }
  }

  private void handleAccept() throws IOException {
    SocketChannel sc = serverSocketChannel.accept();
    sc.configureBlocking(false);
    sc.register(selector, SelectionKey.OP_READ);
    sc.write(ByteBuffer.wrap("checkers:Hello!".getBytes()));
    log.info("Connection Accepted: " + sc.getLocalAddress());
  }

  private void handleClosedConnection(SelectionKey key, SocketChannel sc) {
    String nickToDelete = connectedPlayersMap.getKey(sc);
    if (nickToDelete != null) {
      connectedPlayersMap.remove(nickToDelete, sc);
    }
    for (Iterator<Player> it = hashPlayers.iterator(); it.hasNext(); ) {
      Player player = it.next();
      if (player.getNickname().equals(nickToDelete)) {
        it.remove();
      }
    }
    String uuid = inviteCodeMap.getKey(sc);
    inviteCodeMap.remove(uuid, sc);
    key.cancel();
  }

  private void handleInviteRejected(String msg) throws IOException {
    SocketChannel channel = inviteCodeMap.get(msg);
    channel.write(ByteBuffer.wrap("checkers:inviteRejected".getBytes()));
  }

  private void handleInviteOk(SocketChannel sc, String msg) throws IOException {
    SocketChannel channel = inviteCodeMap.get(msg);
    channel.write(ByteBuffer.wrap(("checkers:inviteOk " + msg).getBytes()));
    sc.keyFor(selector).attach(new Session(channel));
    channel.keyFor(selector).attach(new Session(sc));
  }

  private void handleInviteAsk(SocketChannel sc, String msg) throws IOException {
    if (msg.length() != INVITE_CODE_LENGTH) {
      log.warn(
          String.format(
              "Invalid msg length, is %s should be %s", msg.length(), INVITE_CODE_LENGTH));
      return;
    }
    if (!inviteCodeMap.containsKey(msg)) {
      log.warn("Message code not found");
      sc.write(ByteBuffer.wrap("checkers:inviteErr!".getBytes()));
      return;
    }
    Player play = null;
    SocketChannel dest = inviteCodeMap.get(msg);
    String nick = connectedPlayersMap.getKey(sc);
    for (Player player : hashPlayers) {
      if (player.getNickname().equals(nick)) {
        play = player;
        break;
      }
    }
    if (play == null) {
      log.error("Player is null, not possible");
      return;
    }
    String srcCode = inviteCodeMap.getKey(sc);
    dest.write(
        ByteBuffer.wrap(
            String.format(
                    "checkers:inviteAsk %s %s %s",
                    play.getNickname(), play.getProfileImg(), srcCode)
                .getBytes()));
  }

  private void handleConfig(SocketChannel sc, String msg) throws IOException {
    if (connectedPlayersMap.containsValue(sc)) {
      sc.write(ByteBuffer.wrap("checkers:confUserTaken!".getBytes()));
    }
    Optional<Config> conf = parseConfigMessage(msg.substring(16));
    if (conf.isEmpty()) {
      log.error("Got invalid config message, ignoring");
      return;
    }
    Config config = conf.get();
    if (!Base64.isBase64(config.getNickname())) {
      sc.write(ByteBuffer.wrap("checkers:confBadNick!".getBytes()));
    } else if (!Base64.isBase64(config.getProfileImg())) {
      sc.write(ByteBuffer.wrap("checkers:confBadImg!".getBytes()));
    }
    if (connectedPlayersMap.containsKey(conf.get().getNickname())) {
      log.warn("User already present on server: " + conf.get().getNickname());
      sc.write(ByteBuffer.wrap("checkers:error user already present!".getBytes()));
      return;
    }
    String inviteCode = generateInviteCode();
    inviteCodeMap.put(inviteCode, sc);
    connectedPlayersMap.put(conf.get().getNickname(), sc);
    hashPlayers.add(new Player(conf.get().getNickname(), conf.get().getProfileImg()));
    ByteBuffer src = ByteBuffer.wrap(("checkers:confOk " + inviteCode + "!").getBytes());
    sc.write(src);
  }

  public String generateInviteCode() {
    String code = null;
    for (; ; ) {
      code = UUID.randomUUID().toString().replace("-", "").substring(0, INVITE_CODE_LENGTH);
      if (!inviteCodeMap.containsKey(code)) {
        return code;
      }
    }
  }

  private void handleRandomGame(SelectionKey key, SocketChannel sc) throws IOException {
    if (!connectedPlayersMap.containsValue(sc)) {
      sc.write(ByteBuffer.wrap("checkers:randomMissingConf!".getBytes()));
    }
    key.cancel();
    Optional<List<SocketChannel>> sockets = findPlayersToRandomGame(sc);
    if (sockets.isEmpty()) {
      sc.write(ByteBuffer.wrap("checkers:randomNoPlayers!".getBytes()));
      return;
    }
    List<SocketChannel> tmp = sockets.get();
    SocketChannel scOne = tmp.get(0);
    SocketChannel scTwo = tmp.get(1);

    try {
      scOne.write(ByteBuffer.wrap("checkers:randomStart!".getBytes()));
      scTwo.write(ByteBuffer.wrap("checkers:randomStart!".getBytes()));
      scOne.keyFor(selector).attach(new Session(scTwo));
      scTwo.keyFor(selector).attach(new Session(scOne));
    } catch (IOException e) {
      // TODO add some recovery mechanism ?
      log.error("Exception in handleRandomGame, closing both sides: ", e);
      handleClosedConnection(key, (SocketChannel) key.channel());
      handleClosedConnection(scTwo.keyFor(selector), scTwo);
    }
  }

  private Optional<List<SocketChannel>> findPlayersToRandomGame(SocketChannel src) {
    if (connectedPlayersMap.size() == 1) {
      log.info("Only one player active");
      return Optional.empty();
    }
    if (connectedPlayersMap.size() - playersInGames.size() < 2) {
      log.info("Only one player available");
      return Optional.empty();
    }
    SocketChannel dst = getRandomPlayerChannel(src);
    if (dst == null) {
      return Optional.empty();
    }
    return Optional.of(List.of(src, dst));
  }

  private SocketChannel getRandomPlayerChannel(SocketChannel src) {
    List<SocketChannel> availablePlayers = new ArrayList<>();
    for (SocketChannel sc : connectedPlayersMap.values()) {
      if (!playersInGames.contains(sc) && sc != src) {
        availablePlayers.add(sc);
      }
    }
    return availablePlayers.get(ThreadLocalRandom.current().nextInt(0, availablePlayers.size()));
  }

  private byte[] transformPlayersListIntoBytes() {
    StringBuilder sb = new StringBuilder();
    sb.append("checkers:onlinePlayers ");
    for (Player player : hashPlayers) {
      sb.append(player.toSendable());
      sb.append(" ");
    }
    return sb.toString().strip().getBytes();
  }

  private Optional<Config> parseConfigMessage(String msg) {
    try {
      String[] parts = msg.split(" ");
      // FIXME magic number
      if (parts.length != 2) {
        throw new InvalidAlgorithmParameterException(
            String.format("Invalid len, should be: %s, is %s", 2, parts.length));
      }
      return Optional.of(new Config(parts[0], parts[1]));
    } catch (Exception e) {
      return Optional.empty();
    }
  }
}
