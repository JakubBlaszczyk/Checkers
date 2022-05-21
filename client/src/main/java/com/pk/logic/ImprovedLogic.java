package com.pk.logic;

import com.pk.frontend.checkers.MoveResult;
import com.pk.frontend.checkers.MoveType;
import com.pk.logic.exceptions.IllegalArgument;
import com.pk.logic.exceptions.IndicesNotFound;
import java.util.ArrayList;
import java.util.List;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

@Data
@Slf4j
public class ImprovedLogic implements Logic {

  public ImprovedLogic(Integer size, Integer rowNum) throws IllegalArgument {
    if (!(size % 2 == 0 && size > 6)) {
      throw new IllegalArgument();
    }
    this.board = new ArrayList<>();
    for (int i = 0; i < size; ++i) {
      this.board.add(new ArrayList<>());
      for (int j = 0; j < size; ++j) {
        this.board.get(this.board.size() - 1).add(LogicTile.EMPTY);
      }
    }

    // initialize black and white pawns
    for (int row = 0; row < rowNum; ++row) {
      for (int i = 0, j = 1; i < size; i = i + 2, j = j + 2) {
        if (row % 2 == 0) {
          this.board.get(i).set(row, LogicTile.BLACK_PAWN);
          this.board.get(j).set(size - (row + 1), LogicTile.WHITE_PAWN);
        } else {
          this.board.get(j).set(row, LogicTile.BLACK_PAWN);
          this.board.get(i).set(size - (row + 1), LogicTile.WHITE_PAWN);
        }
      }
    }

    this.turn = LogicTile.BLACK;
    this.blackKillMove = false;
    this.whiteKillMove = false;
  }

  public MoveResult update(Integer newX, Integer newY, Integer oldX, Integer oldY) {
    this.newPiece = this.board.get(newX).get(newY);
    this.oldPiece = this.board.get(oldX).get(oldY);
    this.newX = newX;
    this.newY = newY;
    this.oldX = oldX;
    this.oldY = oldY;
    log.debug("validateRanges(x) {}", validateRanges(newX), newX);
    log.debug("validateRanges(y) {}", validateRanges(newY));
    log.debug("validateTurn() {} actualPiece {}", validateTurn(), this.oldPiece);
    if (Boolean.FALSE.equals(validateRanges(newX))
        || Boolean.FALSE.equals(validateRanges(newY))
        || Boolean.FALSE.equals(validateTurn())) {
      return new MoveResult(MoveType.NONE);
    }
    log.debug("isDiagonalMove() {}", isDiagonalMove());
    log.debug("isOverlappingMove() {}", isOverlappingMove());
    if (Boolean.FALSE.equals(isDiagonalMove()) || Boolean.TRUE.equals(isOverlappingMove())) {
      return new MoveResult(MoveType.NONE);
    }
    Integer distance = calculateDistance();
    log.trace("distance: {}", distance);
    log.debug("validateDistance() {}", validateDistance(distance));
    log.debug("validateDirection() {}", validateDirection());
    log.debug("validateTilesInBetween() {}", validateTilesInBetween(distance));
    if ((!validateDistance(distance)
        || !validateDirection()
        || !validateTilesInBetween(distance))) {
      return new MoveResult(MoveType.NONE);
    }
    updateKillMove(checkForKillMoves(this.turn));
    if (Boolean.TRUE.equals(isNormalMove(distance))) {
      if (Boolean.TRUE.equals(isMandatoryKillMove())) {
        return new MoveResult(MoveType.MANDATORY_KILL);
      }
      checkForKillMoves(this.oldPiece);
      handleNormalMove();
      return new MoveResult(MoveType.NORMAL);
    } else {
      return new MoveResult(MoveType.KILL, handleKillMove(distance));
    }
  }

  private Boolean isMandatoryKillMove() {
    log.debug("blackKillMove: {} whiteKillMove: {}", this.blackKillMove, this.whiteKillMove);
    return (this.turn.isBlack() && this.blackKillMove)
        || (this.turn.isWhite() && this.whiteKillMove);
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    for (List<LogicTile> list : this.board) {
      for (int i = 0; i < this.board.size() * 2 + 1; ++i) {
        builder.append("-");
      }
      builder.append("\n");
      for (LogicTile tile : list) {
        builder.append("|");
        builder.append(tile.toSymbol());
      }
      builder.append("|\n");
    }
    for (int i = 0; i < this.board.size() * 2 + 1; ++i) {
      builder.append("-");
    }
    builder.append("\n");
    return builder.toString();
  }

  private Boolean isDiagonalMove() {
    return this.newX % 2 == this.newY % 2;
  }

  private Boolean isOverlappingMove() {
    return !this.newPiece.isEmpty() || (this.newX.equals(this.oldX) && this.newY.equals(this.oldY));
  }

  private Integer calculateDistance() {
    return Math.abs(this.newX - this.oldX);
  }

  private Boolean validateDistance(Integer distance) {
    if (this.oldPiece.isKing()) {
      return true;
    }
    return distance == 1 || distance == 2;
  }

  private Boolean validateDirection() {
    if (this.oldPiece.isKing()) {
      return true;
    }
    // it depends on premise that was taken
    // if black start from top then this is incorrect
    log.trace("validate direction this.oldPiece {}", this.oldPiece.toString());
    log.debug("direction: {}", this.oldY - this.newY);
    if (this.oldPiece.isBlack()) {
      return this.oldY - this.newY < 0;
    }
    if (this.oldPiece.isWhite()) {
      return this.oldY - this.newY > 0;
    }
    return false;
  }

  private Boolean validateTilesInBetween(Integer distance) {
    Integer offsetX = Integer.compare(this.newX, this.oldX);
    Integer offsetY = Integer.compare(this.newY, this.oldY);
    Integer sum = 0;
    log.trace("offsetX: {} offsetY: {}", offsetX, offsetY);
    // if difference is 2, then I need to take one loop
    for (int i = 1; i < distance; ++i) {
      LogicTile temp = this.board.get(this.oldX + i * offsetX).get(this.oldY + i * offsetY);
      log.trace("compareColors {}", this.turn.compareColors(temp));
      if (this.turn.compareColors(temp)) {
        return false;
      } else if (this.turn.isOppositeColor(temp)) {
        ++sum;
      }
    }
    return sum < 2;
  }

  private Indices findTileInBetween(Integer distance) {
    Integer offsetX = Integer.compare(this.newX, this.oldX);
    Integer offsetY = Integer.compare(this.newY, this.oldY);
    // if difference is 2, then I need to take one loop
    for (int i = 1; i < distance; ++i) {
      LogicTile temp = this.board.get(this.oldX + i * offsetX).get(this.oldY + i * offsetY);
      if (this.turn.compareColors(temp)) {
        throw new IndicesNotFound("This shouldn't be here inside loop");
      } else if (this.turn.isOppositeColor(temp)) {
        return new Indices(this.oldX + i * offsetX, this.oldY + i * offsetY);
      }
    }
    throw new IndicesNotFound("This shouldn't be here");
  }

  private Boolean isNormalMove(Integer distance) {
    Integer offsetX = Integer.compare(this.newX, this.oldX);
    Integer offsetY = Integer.compare(this.newY, this.oldY);
    // if difference is 2, then I need to take one loop
    for (int i = 1; i < distance; ++i) {
      LogicTile temp = this.board.get(this.oldX + i * offsetX).get(this.oldY + i * offsetY);
      if (this.turn.compareColors(temp)) {
        throw new IndicesNotFound("This shouldn't be here");
      } else if (this.turn.isOppositeColor(temp)) {
        return false;
      }
    }
    return true;
  }

  private Boolean validateRanges(Integer index) {
    return index >= 0 && index < this.board.size();
  }

  private Boolean validateTurn() {
    return this.oldPiece.compareColors(this.turn);
  }

  private Boolean validateOneTileForOppositeColor(
      Integer x, Integer y, LogicTile piece, Integer offset, Direction dirX, Direction dirY) {
    log.trace(
        "Opposite Y {} X {} offset {}",
        (y + (offset * dirY.getDirection())),
        (x + (offset * dirX.getDirection())),
        offset);
    return (validateRanges(x + (offset * dirX.getDirection()))
        && validateRanges(y + (offset * dirY.getDirection()))
        && this.board
            .get(x + (offset * dirX.getDirection()))
            .get(y + (offset * dirY.getDirection()))
            .isOppositeColor(piece));
  }

  private Boolean validateOneTileForEmpty(
      Integer x, Integer y, Integer offset, Direction dirX, Direction dirY) {
    log.trace(
        "Empty Y {} X {} offset {}",
        (y + (offset * dirY.getDirection())),
        (x + (offset * dirX.getDirection())),
        offset);
    return (validateRanges(x + (offset * dirX.getDirection()))
        && validateRanges(y + (offset * dirY.getDirection()))
        && this.board
            .get(x + (offset * dirX.getDirection()))
            .get(y + (offset * dirY.getDirection()))
            .isEmpty());
  }

  private Boolean canKill(Integer x, Integer y, LogicTile piece) {
    log.trace("canKill(x: {}, y: {}, piece: {})", x, y, piece);
    return piece.isKing() ? canKingKill(x, y, piece) : canPawnKill(x, y, piece);
  }

  private Boolean canPawnKill(Integer x, Integer y, LogicTile piece) {
    return (validateOneTileForOppositeColor(
                x, y, piece, 1, Direction.LEFT, Direction.toDirection(this.oldPiece.getColor()))
            && validateOneTileForEmpty(
                x, y, 2, Direction.LEFT, Direction.toDirection(this.oldPiece.getColor())))
        || (validateOneTileForOppositeColor(
                x, y, piece, 1, Direction.RIGHT, Direction.toDirection(this.oldPiece.getColor()))
            && validateOneTileForEmpty(
                x, y, 2, Direction.RIGHT, Direction.toDirection(this.oldPiece.getColor())));
  }

  private Boolean canKingKill(Integer x, Integer y, LogicTile piece) {
    return canKingKillDeepSearch(x, y, piece, Direction.LEFT, Direction.UP)
        || canKingKillDeepSearch(x, y, piece, Direction.LEFT, Direction.DOWN)
        || canKingKillDeepSearch(x, y, piece, Direction.RIGHT, Direction.DOWN)
        || canKingKillDeepSearch(x, y, piece, Direction.RIGHT, Direction.UP);
  }

  private Boolean canKingKillDeepSearch(
      Integer x, Integer y, LogicTile piece, Direction dirX, Direction dirY) {
    Integer i = 1;
    while (i < this.board.size() && validateOneTileForEmpty(x, y, i, dirX, dirY)) {
      ++i;
    }
    return validateOneTileForOppositeColor(x, y, piece, i, dirX, dirY)
        && validateOneTileForEmpty(x, y, i + 1, dirX, dirY);
  }

  private Boolean canChangeToKingPiece() {
    if (this.oldPiece.isKing()) {
      return false;
    }
    if (this.oldPiece.isBlack()) {
      if (this.newY == this.board.size() - 1) {
        return true;
      }
    } else {
      if (this.newY == 0) {
        return true;
      }
    }
    return false;
  }

  private void handleNormalMove() {
    this.board.get(oldX).set(oldY, LogicTile.EMPTY);
    if (Boolean.TRUE.equals(canChangeToKingPiece())) {
      this.board.get(this.newX).set(this.newY, this.oldPiece.promote());
    } else {
      this.board.get(newX).set(newY, this.oldPiece);
    }
    log.debug("turn: {}", this.turn.toString());
    updateTurn();
    log.debug("next turn: {}", this.turn.toString());
  }

  private Indices handleKillMove(Integer distance) {
    Indices indices = findTileInBetween(distance);
    this.board.get(indices.getX()).set(indices.getY(), LogicTile.EMPTY);
    this.board.get(oldX).set(oldY, LogicTile.EMPTY);
    if (Boolean.TRUE.equals(canChangeToKingPiece())) {
      this.board.get(newX).set(newY, this.oldPiece.promote());
      log.trace("handleKillMove() newX: {} newY: {} oldPiece: {}", newX, newY, this.oldPiece);
      updateTurn();
      log.trace("handleKillMove() inKingChange turn: {}", this.turn);
    } else {
      this.board.get(newX).set(newY, this.oldPiece);
      if (canKill(this.newX, this.newY, this.oldPiece)) {
        updateKillMove(true);
      } else {
        updateTurn();
        updateKillMove(false);
      }
    }
    log.trace("handleKillMove() turn: {}", this.turn);
    return indices;
  }

  private void updateTurn() {
    this.turn = this.turn.getOppositeTile();
  }

  private void updateKillMove(Boolean value) {
    log.info("updateKillMove: {}", value);
    if (this.turn.isBlack()) {
      this.blackKillMove = value;
    } else if (this.turn.isWhite()) {
      this.whiteKillMove = value;
    }
  }

  private Boolean checkForKillMoves(LogicTile piece) {
    log.trace("checkForKillMoves(piece: {})", piece);
    for (int i = 0; i < this.board.size(); ++i) {
      for (int j = 0; j < this.board.size(); ++j) {
        LogicTile tile = this.board.get(i).get(j);
        log.trace("checkForKillMoves() tile: {}", tile);
        if (!tile.isEmpty() && piece.compareColors(tile) && canKill(i, j, tile)) {
          log.debug("checkForKillMoves() X: {} Y: {} Tile: {}", i, j, tile);
          return true;
        }
      }
    }
    return false;
  }

  LogicTile oldPiece;
  LogicTile newPiece;
  Integer newX;
  Integer newY;
  Integer oldX;
  Integer oldY;
  Boolean whiteKillMove;
  Boolean blackKillMove;

  List<List<LogicTile>> board;
  LogicTile turn;
}
