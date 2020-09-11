// Distance Function
vec3 trans(vec3 p){
    return mod(p, 5.0) - 2.0;
}

float sphere(vec3 p, float s) {
    return length(p) - s;
}

float map(vec3 p) {
	float s = sphere(p, 0.5);
    float pl = p.y + 0.25;
    float d = min(s, pl);
    return d;
}

vec4 qmul(vec4 a, vec4 b) {
    return vec4(
        a.x * b.x - a.y * b.y - a.z * b.z - a.w * b.w,
        a.x * b.y + a.y * b.x - a.z * b.w + a.w * b.z,
        a.x * b.z + a.y * b.w + a.z * b.x - a.w * b.y,
        a.x * b.w - a.y * b.z + a.z * b.y + a.w * b.x
    );
}

#define ITERATIONS 32
vec2 deMandelbulb(vec3 p, float power) {
    vec3 z = p;
    float dr = 2.0;
    float r;
    float cnt;
    for (int i = 0; i < ITERATIONS; i++) {
        r = length(z);
        if (r > 10.0) break;
        float theta = acos(z.y / r);
        float phi = atan(z.z, z.x);
        dr = pow(r, power - 1.0) * power * dr + 1.0;

        float zr = pow(r, power);
        theta = theta * power;
        phi = phi * power;

        z = zr * vec3(sin(theta) * cos(phi), cos(theta), sin(theta) * sin(phi));
        z += p;
        cnt = float(i);
    }
    return vec2(1.0 * log(r) * r / dr, cnt);
}

vec2 dep(vec3 p) {
    return deMandelbulb(trans(p), 1.5+iTime/10.0);
}

vec2 de(vec3 p){
    return deMandelbulb(p, 1.5+iTime/10.0);
}

vec3 CreateRay(vec2 p, vec3 cameraPos, vec3 cameraTarget, float fov) {
    vec3 forward = normalize(cameraTarget - cameraPos);
    vec3 side = normalize(cross(vec3(0.0, 1.0, 0.0), forward));
    vec3 up = normalize(cross(forward, side));
    return normalize(forward * fov + side * p.x + up * p.y);
}

vec3 spectrum(float s){
    s = mod(s,3.0);
    vec3 ans;
    if(s == 0.0){
        ans = vec3(1.0,1.0,0.4);
    }
    if(s == 1.0){
        ans = vec3(0.3,0.3,0.3);
    }
    if(s == 2.0){
        ans = vec3(1.0,0.5,0.2);
    }
    return ans;
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 p = (fragCoord.xy * 2.0 - iResolution.xy) / min(iResolution.x, iResolution.y);
    
    //カメラの位置
    vec3 cameraPos;
    cameraPos = vec3(3.0,3.0,3.0);
    if(iTime > 10.0){
        cameraPos = vec3(3.0,3.0,iTime-7.0);
    }
    if(iTime > 20.0){
        float t = iTime - 20.0;
        vec3 Pos1 = vec3(3.0,3.0,13.0);
        vec3 Pos2 = vec3(3.0,3.0,3.0);
        cameraPos = t*Pos2 + (1.0-t)*Pos1;
    }
    if(iTime > 21.0){
        cameraPos = vec3(3.0,3.0,3.0); 
    }
    
    // カメラの注視点
    vec3 cameraTarget = vec3(0.0);
    
    // シェーディングピクセルのカメラからシーンへのレイ
    vec3 ray = CreateRay(p, cameraPos, cameraTarget, 2.5);

	// レイマーチング
    float t = 0.01;
    vec3 col = vec3(0.0);
    for(int i=0; i<100; i++) {
    	vec3 pos = cameraPos + ray * t;
        vec2 d = dep(pos);
        if(iTime > 21.0) d = de(pos);
        if (d.x < 0.001) {
            // マテリアル毎に色を決定
            vec3 c;
            c = spectrum(d.y);
        	col = vec3(1.0 - float(i) / 70.) * c;
            break;
        }
        t += d.x;
    }
    fragColor = vec4(col,1.0);
}