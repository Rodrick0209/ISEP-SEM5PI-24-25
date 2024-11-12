import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { AuthService } from '../../services/auth.service';
import { ActivatedRoute, Router, RouterModule } from '@angular/router';

@Component({
  selector: 'app-header',
  standalone: true,
  imports: [CommonModule, RouterModule],
  templateUrl: './header.component.html',
  styleUrls: ['./header.component.css']
})
export class HeaderComponent implements OnInit {
  isLoggedIn: boolean = false;

  constructor(private authService: AuthService, private router: Router ) {}

  ngOnInit(): void {
    // Observe o estado de autenticação
    this.authService.isLoggedIn$.subscribe(status => {
      this.isLoggedIn = status;
    });
  }

  logout(): void {
    this.authService.clearToken();
    this.router.navigate(['/home']);
  }

  getUserEmail(): string | null {
    return this.authService.extractEmailFromToken();
  }

  isAdmin(): boolean {
    const role = this.authService.extractRoleFromToken();
    return role === 'admin';
  }

  isDoctorUser(): boolean {
    const role = this.authService.extractRoleFromToken();
    return role === 'doctor';
  }

  isPatientUser(): boolean {
    const role = this.authService.extractRoleFromToken();
    return role === 'patient';
  }

}