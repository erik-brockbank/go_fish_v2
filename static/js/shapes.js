/*
 * Helper library for drawing lure combination shapes
 * Uses canvas and global shape information provided in constants.js
 */


 /*
 * Lookup function for human readable colors to hex values
 */
const COLOR_LOOKUP = {
    "red": "#f44336",
    "green": "#4CAF50",
    "blue": "#008CBA",
    "yellow": "#FFD700"
};

// Color assignment used for texture dots on some shapes
const TEXTURE_COLOR = "purple";


/*
 * Lure class for drawing lure combinations made up of shape classes defined below (circle, triangle, etc.)
 */
Lure = function(top_shape, bottom_shape, top_color, bottom_color, top_texture, bottom_texture) {
    this.top_shape = top_shape;
    this.bottom_shape = bottom_shape;
    this.top_color = top_color;
    this.bottom_color = bottom_color;
    this.top_texture = top_texture;
    this.bottom_texture = bottom_texture;
};

Lure.prototype.drawLure = function(canvasId, sizeConfig) {
    this.drawShape(canvasId, sizeConfig, this.top_shape, this.top_color, this.top_texture, position = "top");
    this.drawShape(canvasId, sizeConfig, this.bottom_shape, this.bottom_color, this.bottom_texture, position = "bottom");
};

Lure.prototype.drawShape = function(canvasId, sizeConfig, shape_str, color_str, texture, position) {
    var shapeObj = this.makeShapeObj(shape_str, color_str, texture, position, sizeConfig);
    var canvas = document.getElementById(canvasId); // unclear why we can't use jquery here
    if (canvas.getContext && shapeObj) {
        var ctx = canvas.getContext("2d");
        shapeObj.draw(ctx)
    }
};

Lure.prototype.makeShapeObj = function(shape_str, color_str, texture, position, sizeConfig) {
    // get color hex val for this shape
    var color = "";
    if (color_str in COLOR_LOOKUP) color = COLOR_LOOKUP[color_str];
    // get appropriate shape class for this shape
    var shapeObj;
    if (shape_str == "circle") shapeObj = new Circle(sizeConfig, position, color, texture);
    if (shape_str == "oval") shapeObj = new Oval(sizeConfig, position, color, texture);
    if (shape_str == "teardrop") shapeObj = new Teardrop(sizeConfig, position, color, texture);
    if (shape_str == "triangle") shapeObj = new Triangle(sizeConfig, position, color, texture);
    if (shape_str == "diamond") shapeObj = new Diamond(sizeConfig, position, color, texture);
    if (shape_str == "star") shapeObj = new Star(sizeConfig, position, color, texture);

    return shapeObj;
};



Circle = function(sizeConfig, position, color, texture) {
    // TODO is there a cleaner way to do the x, y, radius assignment for each sizeConfig?
    if (sizeConfig == "evidence" || sizeConfig == "prediction") {
        if (position == "top") {
            this.x = 75;
            this.y = 75;
            this.radius = 25;
        } else if (position == "bottom") {
            this.x = 75;
            this.y = 110;
            this.radius = 10;
        }
    }
    if (sizeConfig == "observations") {
        if (position == "top") {
            this.x = 40;
            this.y = 55;
            this.radius = 25;
        } else if (position == "bottom") {
            this.x = 40;
            this.y = 90;
            this.radius = 10;
        }
    }
    if (sizeConfig == "generate" || sizeConfig == "memory") {
        if (position == "top") {
            this.x = 30;
            this.y = 55;
            this.radius = 25;
        } else if (position == "bottom") {
            this.x = 30;
            this.y = 90;
            this.radius = 10;
        }
    }
    this.color = color;
    this.texture = texture;
};

Circle.prototype.draw = function(ctx) {
    ctx.beginPath();
    ctx.arc(this.x, this.y, this.radius, 0, Math.PI * 2);
    ctx.fillStyle = this.color;
    ctx.fill();

    if (this.texture) {
        ctx.beginPath();
        ctx.arc(this.x + 4, this.y + 5, this.radius / 4, 0, Math.PI * 2);
        ctx.fillStyle = TEXTURE_COLOR;
        ctx.fill();
    }
};


Oval = function(sizeConfig, position, color, texture) {
    if (sizeConfig == "evidence" || sizeConfig == "prediction") {
        if (position == "top") {
            this.x = 75;
            this.y = 70;
            this.width = 15;
            this.height = 30;
        } else if (position == "bottom") {
            this.x = 75;
            this.y = 115;
            this.width = 7.5;
            this.height = 15;
        }
    }
    if (sizeConfig == "observations") {
        if (position == "top") {
            this.x = 40;
            this.y = 50;
            this.width = 15;
            this.height = 30;
        } else if (position == "bottom") {
            this.x = 40;
            this.y = 95;
            this.width = 7.5;
            this.height = 15;
        }
    }
    if (sizeConfig == "generate" || sizeConfig == "memory") {
        if (position == "top") {
            this.x = 30;
            this.y = 50;
            this.width = 15;
            this.height = 30;
        } else if (position == "bottom") {
            this.x = 30;
            this.y = 95;
            this.width = 7.5;
            this.height = 15;
        }
    }
    this.color = color;
    this.texture = texture;
};

Oval.prototype.draw = function(ctx) {
    ctx.beginPath();
    ctx.ellipse(this.x, this.y, radiusX = this.width, radiusY = this.height,
        rotation = 0, startAngle = 0, endAngle = Math.PI * 2);
    ctx.fillStyle = this.color;
    ctx.fill();

    if (this.texture) {
        ctx.beginPath();
        ctx.arc(this.x - 2, this.y, this.width / 3, 0, Math.PI * 2);
        ctx.fillStyle = TEXTURE_COLOR;
        ctx.fill();
    }
};


Teardrop = function(sizeConfig, position, color, texture) {
    if (sizeConfig == "evidence" || sizeConfig == "prediction") {
        if (position == "top") {
            this.top_x = 50;
            this.top_y = 50;
            this.width = 50;
        } else if (position == "bottom") {
            this.top_x = 65;
            this.top_y = 110;
            this.width = 20;
        }
    }
    if (sizeConfig == "observations") {
        if (position == "top") {
            this.top_x = 15;
            this.top_y = 30;
            this.width = 50;
        } else if (position == "bottom") {
            this.top_x = 30;
            this.top_y = 90;
            this.width = 20;
        }
    }
    if (sizeConfig == "generate" || sizeConfig == "memory") {
        if (position == "top") {
            this.top_x = 5;
            this.top_y = 30;
            this.width = 50;
        } else if (position == "bottom") {
            this.top_x = 20;
            this.top_y = 90;
            this.width = 20;
        }
    }
    this.color = color;
    this.texture = texture;
};

Teardrop.prototype.draw = function(ctx) {
    ctx.beginPath();
    ctx.arc(this.top_x + (this.width / 2), this.top_y + (this.width / 2), this.width / 2, (5/4) * Math.PI, (7/4) * Math.PI, true);
    ctx.lineTo(this.top_x + (this.width / 2), this.top_y - (this.width / 2));
    ctx.closePath();
    ctx.fillStyle = this.color;
    ctx.fill();

    if (this.texture) {
        ctx.beginPath();
        ctx.arc(this.top_x + 16, this.top_y + 8, this.width / 8, 0, Math.PI * 2);
        ctx.fillStyle = TEXTURE_COLOR;
        ctx.fill();
    }
};


Triangle = function(sizeConfig, position, color, texture) {
    if (sizeConfig == "evidence" || sizeConfig == "prediction") {
        if (position == "top") {
            this.top_x = 50;
            this.top_y = 50;
            this.base = 50;
            this.height = 50;
        } else if (position == "bottom") {
            this.top_x = 62.5;
            this.top_y = 100;
            this.base = 25;
            this.height = 25;
        }
    }
    if (sizeConfig == "observations") {
        if (position == "top") {
            this.top_x = 15;
            this.top_y = 30;
            this.base = 50;
            this.height = 50;
        } else if (position == "bottom") {
            this.top_x = 27.5;
            this.top_y = 80;
            this.base = 25;
            this.height = 25;
        }
    }
    if (sizeConfig == "generate" || sizeConfig == "memory") {
        if (position == "top") {
            this.top_x = 5;
            this.top_y = 30;
            this.base = 50;
            this.height = 50;
        } else if (position == "bottom") {
            this.top_x = 17.5;
            this.top_y = 80;
            this.base = 25;
            this.height = 25;
        }
    }
    this.color = color;
    this.texture = texture;
};

Triangle.prototype.draw = function(ctx) {
    ctx.beginPath();
    ctx.moveTo(this.top_x, this.top_y);
    ctx.lineTo(this.top_x + this.base, this.top_y);
    ctx.lineTo(this.top_x + (this.base / 2), this.top_y + this.height);
    ctx.closePath();
    ctx.fillStyle = this.color;
    ctx.fill();

    if (this.texture) {
        ctx.beginPath();
        ctx.arc(this.top_x + 12, this.top_y + 8, this.base / 10, 0, Math.PI * 2);
        ctx.fillStyle = TEXTURE_COLOR;
        ctx.fill();
    }
};


Diamond = function(sizeConfig, position, color, texture) {
    if (sizeConfig == "evidence" || sizeConfig == "prediction") {
        if (position == "top") {
            this.top_x = 75;
            this.top_y = 50;
            this.width = 50;
            this.height = 50;
        } else if (position == "bottom") {
            this.top_x = 75;
            this.top_y = 100;
            this.width = 25;
            this.height = 25;
        }
    }
    if (sizeConfig == "observations") {
        if (position == "top") {
            this.top_x = 40;
            this.top_y = 30;
            this.width = 50;
            this.height = 50;
        } else if (position == "bottom") {
            this.top_x = 40;
            this.top_y = 80;
            this.width = 25;
            this.height = 25;
        }
    }
    if (sizeConfig == "generate" || sizeConfig == "memory") {
        if (position == "top") {
            this.top_x = 30;
            this.top_y = 30;
            this.width = 50;
            this.height = 50;
        } else if (position == "bottom") {
            this.top_x = 30;
            this.top_y = 80;
            this.width = 25;
            this.height = 25;
        }
    }
    this.color = color;
    this.texture = texture;
};

Diamond.prototype.draw = function(ctx) {
    ctx.beginPath();
    ctx.moveTo(this.top_x, this.top_y);
    ctx.lineTo(this.top_x - (this.width / 2), this.top_y + (this.height / 2));
    ctx.lineTo(this.top_x, this.top_y + this.height);
    ctx.lineTo(this.top_x + (this.width / 2), this.top_y + (this.height / 2));
    ctx.closePath();
    ctx.fillStyle = this.color;
    ctx.fill();

    if (this.texture) {
        ctx.beginPath();
        ctx.arc(this.top_x + 4, this.top_y + 12, this.width / 10, 0, Math.PI * 2);
        ctx.fillStyle = TEXTURE_COLOR;
        ctx.fill();
    }
};


Star = function(sizeConfig, position, color, texture) {
    if (sizeConfig == "evidence" || sizeConfig == "prediction") {
        if (position == "top") {
            this.x = 75;
            this.y = 70;
            this.outer_radius = 30;
            this.inner_radius = 15;
        } else if (position == "bottom") {
            this.x = 75;
            this.y = 115;
            this.outer_radius = 15;
            this.inner_radius = 7.5;
        }
    }
    if (sizeConfig == "observations") {
        if (position == "top") {
            this.x = 40;
            this.y = 50;
            this.outer_radius = 30;
            this.inner_radius = 15;
        } else if (position == "bottom") {
            this.x = 40;
            this.y = 95;
            this.outer_radius = 15;
            this.inner_radius = 7.5;
        }
    }
    if (sizeConfig == "generate" || sizeConfig == "memory") {
        if (position == "top") {
            this.x = 30;
            this.y = 50;
            this.outer_radius = 30;
            this.inner_radius = 15;
        } else if (position == "bottom") {
            this.x = 30;
            this.y = 95;
            this.outer_radius = 15;
            this.inner_radius = 7.5;
        }
    }
    this.spikes = 4;
    this.color = color;
    this.texture = texture;
};

Star.prototype.draw = function(ctx) {
    var rot = Math.PI / 2 * 3;
    var x = this.x;
    var y = this.y;
    var step = Math.PI / this.spikes;

    ctx.beginPath();
    ctx.moveTo(this.x, this.y - this.outer_radius)
    for (i = 0; i < this.spikes; i++){
        x = this.x + Math.cos(rot) * this.outer_radius;
        y = this.y + Math.sin(rot) * this.outer_radius;
        ctx.lineTo(x, y);
        rot += step;

        x = this.x + Math.cos(rot) * this.inner_radius;
        y = this.y + Math.sin(rot) * this.inner_radius;
        ctx.lineTo(x, y);
        rot += step;
    }
    ctx.lineTo(this.x, this.y - this.outer_radius);
    ctx.closePath();
    ctx.fillStyle = this.color;
    ctx.fill();

    if (this.texture) {
        ctx.beginPath();
        ctx.arc(this.x + 2, this.y - 2, this.inner_radius / 3, 0, Math.PI * 2);
        ctx.fillStyle = TEXTURE_COLOR;
        ctx.fill();
    }
};
