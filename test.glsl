// Distance Function

vec3 trans(vec3 p){
    return mod(p, 3.0) - 1.0;
}

float sphere(vec3 p, float s) {
    return length(p) - s;
}

float sdCapsule( vec3 p, vec3 a, vec3 b, float r )
{
  vec3 pa = p - a, ba = b - a;
  float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
  return length( pa - ba*h ) - r;
}

vec3 foldX(vec3 p){
    p.x = abs(p.x);
    return p;
}

float map(vec3 p) {
	float s = sphere(p, 0.5);
    float c = sdCapsule(foldX(p), vec3(0.2,0.1,-.5),
                        vec3(0.2,0.1,-.5),0.1);
    float d = sdCapsule(p, vec3(0.2,-.1, -0.5),
                        vec3(-.2,-.1,-.5),0.05);
    float e = sdCapsule(p, vec3(-.1,.3, -0.5),
                        vec3(-.3,.3,-.5),0.05);
    float f = sdCapsule(p, vec3(.1,.2, -0.5),
                        vec3(.3,.2,-.5),0.05);
    float ans = min(s,c);
    ans = min(ans,d);
    ans = min(ans,e);
    ans = min(ans,f);
    return ans;
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
    vec3 cameraPos = vec3(0.0, 0.0, -2.0);
    
    // カメラの注視点
    vec3 cameraTarget = vec3(0.0);
    
    // シェーディングピクセルのカメラからシーンへのレイ
    vec3 ray = CreateRay(p, cameraPos, cameraTarget, 2.5);

	// レイマーチング
    float t = 0.01;
    vec3 col = vec3(0.0);
    for(int i=0; i<99; i++) {
    	vec3 pos = cameraPos + ray * t;
        float d = map(pos);
        if (d < 0.001) {
        	col = vec3(1.0 - float(i) / 70.);
            break;
        }
        t += d;
    }
    fragColor = vec4(col,1.0);
}