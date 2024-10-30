// auth.service.ts
import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

interface LoginResponse {
  // Define response structure if known, or use 'any' if unknown
  // Example: token: string;
}

@Injectable({
  providedIn: 'root'
})
export class AuthService {
  private loginUrl = '/api/Login/login';

  constructor(private http: HttpClient) { }

  login(email: string, password: string): Observable<LoginResponse> {
    const body = { email, password };
    return this.http.post<LoginResponse>(this.loginUrl, body);
  }
}