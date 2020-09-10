// Distance Function
float sphere(vec3 p, float s) {
    return length(p) - s;
}

float sdBoundingBox( vec3 p, vec3 b, float e, float phi)
{
    p.y -= 1.0;
    float t = iTime/10.0;
    mat3 rotate = mat3 (
        sin(t*phi),cos(t*phi),0.0,
        -cos(t*phi),sin(t*phi),0.0,
        0.0,0.0,1.0
    );
    mat3 phi_rotate = mat3(
        sin(phi),0.0,cos(phi),
        0.0,1.0,0.0,
        -cos(phi),0.0,sin(phi)
    );
    p = phi_rotate * rotate * p;
    p = abs(p  )-b;
    vec3 q = abs(p+e)-e;
  return min(min(
      length(max(vec3(p.x,q.y,q.z),0.0))+min(max(p.x,max(q.y,q.z)),0.0),
      length(max(vec3(q.x,p.y,q.z),0.0))+min(max(q.x,max(p.y,q.z)),0.0)),
      length(max(vec3(q.x,q.y,p.z),0.0))+min(max(q.x,max(q.y,p.z)),0.0));
}

float map(vec3 p) {
    float ans = 1001001001.0;
    for(float i=1.0;i<4.0;i += 0.1){
        float tmp = sdBoundingBox(p, vec3(i,i,i), 0.1, i);
        ans = min(ans,tmp);
    }
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
        float d = map(pos);
        if (d < 0.001) {
        	col = vec3(1.0 - float(i) / 70.);
            break;
        }
        t += d;
    }
    fragColor = vec4(col,1.0);
}