package com.pk.logic;

import java.util.ArrayList;
import java.util.List;

import com.pk.frontend.checkers.MoveType;
import com.pk.logic.exceptions.IllegalConstructorException;
import com.pk.logic.exceptions.IndicesNotFound;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

@Data
@Slf4j
public class ImprovedLogic implements Logic {

  public ImprovedLogic(Integer size) throws IllegalConstructorException {
    if (!(size % 2 == 0 && size > 6)) {
      throw new IllegalConstructorException();
    }
    this.board = new ArrayList<>();
    for (int i = 0; i < size; ++i) {
      this.board.add(new ArrayList<>());
      for (int j = 0; j < size; ++j) {
        this.board.get(this.board.size() - 1).add(LogicTile.EMPTY);
      }
    }

    // initialize black and white pawns
    for (int i = 0, j = 1; i < size; i = i + 2, j = j + 2) {
      this.board.get(i).set(0, LogicTile.BLACK_PAWN);
      this.board.get(j).set(1, LogicTile.BLACK_PAWN);
      this.board.get(i).set(size - 2, LogicTile.WHITE_PAWN);
      this.board.get(j).set(size - 1, LogicTile.WHITE_PAWN);
    }

    turn = LogicTile.BLACK;
  }

  private Boolean isDiagonalMove() {
    return this.newX % 2 == this.newY % 2;
  }

  private Boolean isOverlappingMove(Integer oldX, Integer oldY) {
    return !this.newPiece.isEmpty() || (this.newX.equals(oldX) && this.newY.equals(oldY));
  }

  private Integer calculateDistance(Integer oldX) {
    return Math.abs(this.newX - oldX);
  }

  private Boolean validateDistance(Integer distance) {
    if (this.oldPiece.isKing()) {
      return true;
    }
    return distance == 1 || distance == 2;
  }

  private Boolean validateDirection(Integer oldY) {
    if (this.oldPiece.isKing()) {
      return true;
    }
    // it depends on premise that was taken
    // if black start from top then this is incorrect
    log.debug("validate direction this.oldPiece {}", this.oldPiece.toString());
    if (this.oldPiece.isBlack()) {
      return oldY - this.newY < 0;
    }
    if (this.oldPiece.isWhite()) {
      return oldY - this.newY > 0;
    }
    return false;
  }

  private Boolean validateTilesInBetween(Integer oldX, Integer oldY, Integer distance) {
    Integer offsetX = Integer.compare(this.newX, oldX);
    Integer offsetY = Integer.compare(this.newY, oldY);
    Integer sum = 0;
    log.debug("offsetX: {} offsetY: {}", offsetX, offsetY);
    // if difference is 2, then I need to take one loop
    for (int i = 1; i < distance; ++i) {
      LogicTile temp = this.board.get(oldX + i * offsetX).get(oldY + i * offsetY);
      log.debug("compareColors {}", this.turn.compareColors(temp));
      if (this.turn.compareColors(temp)) {
        return false;
      } else if (this.turn.isOppositeColor(temp)) {
        ++sum;
      }
    }
    return sum < 2;
  }

  private Indices findTileInBetween(Integer oldX, Integer oldY, Integer distance) {
    Integer offsetX = Integer.compare(this.newX, oldX);
    Integer offsetY = Integer.compare(this.newY, oldY);
    // if difference is 2, then I need to take one loop
    for (int i = 1; i < distance; ++i) {
      LogicTile temp = this.board.get(oldX + i * offsetX).get(oldY + i * offsetY);
      if (this.turn.compareColors(temp)) {
        throw new IndicesNotFound("This shouldn't be here inside loop");
      } else if (this.turn.isOppositeColor(temp)) {
        return new Indices(oldX + i * offsetX, oldY + i * offsetY);
      }
    }
    throw new IndicesNotFound("This shouldn't be here");
  }

  public MoveType update(Integer newX, Integer newY, Integer oldX, Integer oldY) {
    this.newPiece = this.board.get(newX).get(newY);
    this.oldPiece = this.board.get(oldX).get(oldY);
    this.newX = newX;
    this.newY = newY;
    log.debug("isDiagonalMove() {}", isDiagonalMove());
    log.debug("isOverlappingMove() {}", isOverlappingMove(oldX, oldY));
    if (Boolean.FALSE.equals(isDiagonalMove()) || Boolean.TRUE.equals(isOverlappingMove(oldX, oldY))) {
      return MoveType.NONE;
    }
    Integer distance = calculateDistance(oldX);
    log.debug("validateDistance() {}", validateDistance(distance));
    log.debug("validateDirection() {}", validateDirection(oldY));
    log.debug("validateTilesInBetween() {}", validateTilesInBetween(oldX, oldY, distance));
    if ((!validateDistance(distance) || !validateDirection(oldY) || !validateTilesInBetween(oldX, oldY, distance))) {
      return MoveType.NONE;
    }
    log.debug("distance: {}", distance);
    if (distance == 1) {
      this.board.get(oldX).set(oldY, LogicTile.EMPTY);
      this.board.get(newX).set(newY, this.oldPiece);
      this.turn = this.turn.isWhite() ? LogicTile.BLACK : LogicTile.WHITE;
      log.debug("turn: {}", this.turn.toString());
      return MoveType.NORMAL;
    } else if (distance == 2) {
      Indices indices = findTileInBetween(oldX, oldY, distance);
      this.board.get(indices.getX()).set(indices.getY(), LogicTile.EMPTY);
      this.board.get(oldX).set(oldY, LogicTile.EMPTY);
      this.board.get(newX).set(newY, this.oldPiece);
      return MoveType.KILL;
    }
    return MoveType.NONE;
  }

  LogicTile oldPiece;
  LogicTile newPiece;
  Integer newX;
  Integer newY;

  List<List<LogicTile>> board;
  LogicTile turn;
}
