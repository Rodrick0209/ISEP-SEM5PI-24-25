import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable, BehaviorSubject, firstValueFrom } from 'rxjs';

interface LoginResponse {
  token: string;
}

interface GoogleResponse {
  token: {
    result: string
  };
}

@Injectable({
  providedIn: 'root'
})
export class AuthService {
  private loginUrl = '/api/Login/login';
  private loggedIn = new BehaviorSubject<boolean>(this.isLoggedIn()); // Inicializa com o estado atual de login
  isLoggedIn$ = this.loggedIn.asObservable();
  private googleVerifyUrl = '/api/Login/google-response'; // Add your backend endpoint here
  private bypassInterceptorFlag = false;


  constructor(private http: HttpClient) { }

  setBypassInterceptor(bypass: boolean): void {
    this.bypassInterceptorFlag = bypass;
  }

  getBypassInterceptor(): boolean {
    return this.bypassInterceptorFlag;
  }

  login(email: string, password: string): Observable<LoginResponse> {
    const body = { email, password };
    return this.http.post<LoginResponse>(this.loginUrl, body);
  }

  verifyGoogleToken(): Observable<GoogleResponse> {
    return this.http.get<GoogleResponse>(this.googleVerifyUrl);
  }

  saveToken(token: string): void {
    console.log('Saving login token', token);
    localStorage.setItem('token', token);
    this.loggedIn.next(true); // Atualiza o estado para "logado"
  }

  getToken(): string | null {
    return localStorage.getItem('token');
  }

  isLoggedIn(): boolean {
    return this.getToken() !== null;
  }

  clearToken(): void {
    localStorage.removeItem('token');
    this.loggedIn.next(false); // Atualiza o estado para "não logado"
  }

  extractEmailFromToken(): string | null {
    const token = this.getToken();
    if (!token) {
      return null;
    }

    try {
      const payload = JSON.parse(atob(token.split('.')[1]));
      return payload.email || null;
    } catch (e) {
      console.error('Invalid token format', e);
      return null;
    }
  }

  extractRoleFromToken(): string | null {
    const token = this.getToken();
    if (!token) {
      return null;
    }

    try {
      const payload = JSON.parse(atob(token.split('.')[1]));
      return payload.role || null;
    } catch (e) {
      console.error('Invalid token format', e);
      return null;
    }
  }

  async validatePassword(email: string, password: string): Promise<boolean> {
    try {
      const response = await firstValueFrom(this.login(email, password));

      console.log('API Response:', response);

      if (response && response.token) {
        return true; // Validation successful
      } else {
        console.error('Validation failed: Token not found in response');
        return false; // Validation failed
      }
    } catch (error) {
      console.error('Error validating password:', error || error);
      return false; // API call failed
    }
  }
}