#include <iostream>
#include <cmath>
using namespace std;

void render_frame(double A, double B) {
    double cosA = cos(A);
    double sinA = sin(A);
    double cosB = cos(B);
    double sinB = sin(B);
    const int screen_width = 35;
    const int screen_height = 35;
    const double theta_spacing = 0.07;
    const double phi_spacing = 0.02;
    const double R1 = 1;
    const double R2 = 2;
    const double K2 = 5;
    const double K1 = screen_width * K2 * 3 / (8 * (R1 + R2));
    char char_output[screen_height + 1][screen_width + 1];
    double zbuffer[screen_height + 1][screen_width + 1];

    for (int i = 0; i <= screen_height; i++) {
        for (int j = 0; j <= screen_width; j++) {
            char_output[i][j] = ' ';
            zbuffer[i][j] = 0;
        }
    }

    double theta = 0;
    while (theta < 2 * M_PI) {
        theta += theta_spacing;
        double costheta = cos(theta);
        double sintheta = sin(theta);
        double phi = 0;
        while (phi < 2 * M_PI) {
            phi += phi_spacing;
            double cosphi = cos(phi);
            double sinphi = sin(phi);
            double circlex = R2 + R1 * costheta;
            double circley = R1 * sintheta;
            double x = circlex * (cosB * cosphi + sinA * sinB * sinphi) - circley * cosA * sinB;
            double y = circlex * (sinB * cosphi - sinA * cosB * sinphi) + circley * cosA * cosB;
            double z = K2 + cosA * circlex * sinphi + circley * sinA;
            double ooz = 1 / z;
            int xp = int(screen_width / 2 + K1 * ooz * x);
            int yp = int(screen_height / 2 - K1 * ooz * y);
            double L = cosphi * costheta * sinB - cosA * costheta * sinphi - sinA * sintheta + cosB * (cosA * sintheta - costheta * sinA * sinphi);
            if (L > 0) {
                if (ooz > zbuffer[xp][yp]) {
                    zbuffer[xp][yp] = ooz;
                    int luminance_index = int(L * 8);
                    char_output[xp][yp] = ".,-~:;=!*#$@"[luminance_index];
                }
            }
        }
    }

    cout << "\x1b[H";
    for (int i = 0; i < screen_height; i++) {
        for (int j = 0; j < screen_width; j++) {
            cout << char_output[i][j];
        }
        cout << endl;
    }
}

int main() {
    cout << "\x1b[2J";
    double A = 1.0;
    double B = 1.0;
    for (int i = 0; i < 250; i++) {
        render_frame(A, B);
        A += 0.08;
        B += 0.03;
    }
    return 0;
}