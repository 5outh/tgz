export class Rect {
  constructor(x, y, w, h) {
    this.x = x;
    this.y = y;
    this.w = w;
    this.h = h;
  }

  center() {
    return [x + w/2, y + h/2];
  }

  drawRounded(ctx, r) {
    let {x,y,w,h} = this;

    if (w < 2 * r) r = w / 2;
    if (h < 2 * r) r = h / 2;

    ctx.beginPath();
    ctx.moveTo(x+r, y);
    ctx.arcTo(x+w, y,   x+w, y+h, r);
    ctx.arcTo(x+w, y+h, x,   y+h, r);
    ctx.arcTo(x,   y+h, x,   y,   r);
    ctx.arcTo(x,   y,   x+w, y,   r);
    ctx.closePath();
  }

  draw(ctx) {
    let {x,y,w,h} = this;
    ctx.rect(x,y,w,h);
  }

  drawX(ctx) {
    let {x,y,w,h} = this;

    ctx.beginPath();
    ctx.moveTo(x,y);
    ctx.lineTo(x+w,y+h);

    ctx.moveTo(x+w,y);
    ctx.lineTo(x,y+w);
    ctx.closePath();
  }

  scale(amount) {
    let {x,y,w,h} = this;
    const [newWidth, newHeight] = [w * amount, h * amount]
    let [offsetX, offsetY] = [(w - newWidth) / 2, (h - newHeight) / 2]
    return new Rect(x+offsetX,y+offsetY,newWidth,newHeight);
  }
}
