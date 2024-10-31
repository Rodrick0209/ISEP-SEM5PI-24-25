import { Component, ElementRef, ViewChild } from '@angular/core';
import { AuthService } from './auth.service';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { HttpClientModule } from '@angular/common/http'; // Import HttpClientModule
import { Router, RouterModule } from '@angular/router';

@Component({
  selector: 'app-login',
  standalone: true,
  imports: [FormsModule, CommonModule, HttpClientModule, RouterModule], // Add HttpClientModule to imports
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css']
})
export class LoginComponent {
  @ViewChild('myCanvas') private canvasRef!: ElementRef;

  // Propriedades do Login
  email: string = '';
  password: string = '';
  errorMessage: string | null = null;

  constructor(private authService: AuthService, private router: Router) { }

  onLogin() {
    this.authService.login(this.email, this.password).subscribe(response => {
      this.authService.saveToken(response.token); // Salva apenas o token
      // Redirecionar ou atualizar a interface após o login
      this.router.navigate(['/home']);
    }, error => {
      console.error('Erro no login:', error);
    });
  }
}