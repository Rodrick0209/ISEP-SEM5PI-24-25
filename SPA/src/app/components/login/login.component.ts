import { Component, ElementRef, ViewChild } from '@angular/core';
import { AuthService } from '../../services/auth.service';
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
    this.authService.login(this.email, this.password).subscribe(
      response => {
        if (response.token) {
          this.authService.saveToken(response.token); // Salva o token
          // Redirecionar ou atualizar a interface após o login
          this.router.navigate(['/home']);
        } else {
          // Defina a mensagem de erro se o token não estiver presente
          this.errorMessage = 'Login falhou. Por favor, tente novamente.';
        }
      },
      error => {
        console.error('Erro no login:', error);
        this.errorMessage = 'Erro no login: ' + (error.error?.message || 'Por favor, verifique suas credenciais.');
      }
    );
  }
}