#define MAT_SPONGE 0.0
#define MAT_CREAM 1.0
#define MAT_BALL 2.0
#define MAT_CANDLE 3.0
#define MAT_DISH 4.0
#define MAT_FRAME 5.0
#define MAT_OUTSIDE 6.0
#define MAT_INSIDE 7.0
#define MAT_HEART 8.0
#define MAT_SHELF 9.0
#define MAT_FLOOR 10.0
#define MAT_CORE 11.0

const float pi = acos(-1.);
const float pi2 = pi * 2.0;

// Distance Function
vec3 trans(vec3 p){
    return mod(p, 10.0) - 2.0;
}

float sphere(vec3 p, float s) {
    return length(p) - s;
}

vec2 opU(vec2 d1, vec2 d2)
{
	return (d1.x<d2.x) ? d1 : d2;
}

float sdSphere(vec3 p, float s)
{
    return length(p) - s;
}

float sdRoundedCylinder( vec3 p, float ra, float rb, float h )
{
    vec2 d = vec2( length(p.xz)-2.0*ra+rb, abs(p.y) - h );
    return min(max(d.x,d.y),0.0) + length(max(d,0.0)) - rb;
}

mat2 rot( float th ){ vec2 a = sin(vec2(1.5707963, 0) + th); return mat2(a, -a.y, a.x); }

vec2 pMod(in vec2 p, in float s) {
    float a = pi / s - atan(p.x, p.y);
    float n = pi2 / s;
    a = floor(a / n) * n;
    p *= rot(a);
    return p;
}

float smin( float d1, float d2, float k ) {
    float h = clamp( 0.5 + 0.5*(d2-d1)/k, 0.0, 1.0 );
    return mix( d2, d1, h ) - k*h*(1.0-h); }

float sdCream(vec3 p, float phiScale, float radiusOffset, float thickness)
{
    p.zx = vec2(atan(p.x, p.z) / pi * phiScale, length(p.zx));
    p.x -= radiusOffset;

    vec2 theta = vec2(1.6, 0.) + p.z * pi * 4.;
    float k1 = length(p.yx + sin(theta) * 2.) - thickness;
    float k2 = length(p.yx + sin(theta + pi) * 2.) - thickness;

    return smin(k1, k2, 2.5);
}

float sdDish(vec3 p)
{
    vec2 q = vec2(atan(p.z, p.x), length(p.xz));
    float d = q.y - 1.2 - sin(q.x * 16.0) * 0.04;
    vec2 w = vec2( d, abs(p.y) - 0.03);
    return min(max(w.x,w.y),0.0) + length(max(w,0.0));
}


float sdCappedCylinder( vec3 p, vec2 h )
{
  vec2 d = abs(vec2(length(p.xz),p.y)) - h;
  return min(max(d.x,d.y),0.0) + length(max(d,0.0));
}

vec2 sdCake(vec3 p)
{
    p = trans(p);

    float ss = sdSphere(p, 2.0);
    if (ss > 1.0) {
    	return vec2(ss, 0.0);
    }
    p.y += 0.15;
    vec3 q = p;
    q.y = abs(p.y) - 0.2;
    vec2 sponge = vec2(sdRoundedCylinder(q, 0.5, 0.2, 0.15), MAT_SPONGE);
    
    q = p;
    q.xz = pMod(q.xz, 8.0);
    q.yz -= vec2(0.58, 0.6);
    vec3 s = vec3(50.0, 75.0, 50.0);
    vec2 cream = vec2(sdCream(q * s, 1., 3.0, 2.0) / s.y * 0.75, MAT_CREAM);
    q.y -= 0.06;
    vec2 redBall = vec2(sdSphere(q, 0.08), MAT_BALL);
    
    q = p;
    q.y -= 0.8;
    vec2 candle = vec2(sdCappedCylinder(q, vec2(0.03, 0.4)), MAT_CANDLE);
    
    q.y += 1.25;
    vec2 dish = vec2(sdDish(q), MAT_DISH);
    
    vec2 d = opU(sponge, cream);
    d = opU(redBall, d);
    d = opU(candle, d);
    d = opU(dish, d);
    
    return d;
}

vec2 map(vec3 p) {
    return sdCake(p);
}

vec3 CreateRay(vec2 p, vec3 cameraPos, vec3 cameraTarget, float fov) {
    vec3 forward = normalize(cameraTarget - cameraPos);
    vec3 side = normalize(cross(vec3(0.0, 1.0, 0.0), forward));
    vec3 up = normalize(cross(forward, side));
    return normalize(forward * fov + side * p.x + up * p.y);
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 p = (fragCoord.xy * 2.0 - iResolution.xy) / min(iResolution.x, iResolution.y);
    
    //カメラの位置
    vec3 cameraPos = vec3(sin(iTime) * 5.0, 1, cos(iTime) * 5.0);
    cameraPos = vec3(10,3,10);
    
    // カメラの注視点
    vec3 cameraTarget = vec3(0.0);
    
    // シェーディングピクセルのカメラからシーンへのレイ
    vec3 ray = CreateRay(p, cameraPos, cameraTarget, 2.5);

	// レイマーチング
    float t = 0.01;
    vec3 col = vec3(0.0);
    for(int i=0; i<99; i++) {
    	vec3 pos = cameraPos + ray * t;
        vec2 d = map(pos);
        if (d.x < 0.001) {
            // マテリアル毎に色を決定
            vec3 c;
            if (d.y == MAT_SPONGE) {
            	c = vec3(1.0, 1.0, 0.6);
            } else if (d.y == MAT_CREAM) {
            	c = vec3(1.0, 1.0, 1.0);
            } else if (d.y == MAT_BALL){
                c = vec3(1.0, 0.0, 0.0);
            } else if (d.y == MAT_CANDLE){
                c = vec3(0.0, 1.0, 0.0);
            } else if (d.y == MAT_DISH){
                c = vec3(1.0, 1.0, 1.0);
                
            } else{
                c = vec3(1.0, 1.0, 1.0);
            }
        	col = vec3(1.0 - float(i) / 100.) * c;
            break;
        }
        t += d.x;
    }
    fragColor = vec4(col,1.0);
}