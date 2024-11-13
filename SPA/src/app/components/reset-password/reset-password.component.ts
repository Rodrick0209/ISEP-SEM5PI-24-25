import { Component, OnInit } from '@angular/core';
import { UserService } from '../../services/user.service';
import { ActivatedRoute, Router } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';

@Component({
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './reset-password.component.html',
  styleUrls: ['./reset-password.component.css']
})
export class ResetPasswordComponent implements OnInit {
  email: string = '';
  errorMessage: string | null = null;
  successMessage: string | null = null;
  newPassword: string = '';
  confirmPassword: string = '';
  token: string | null = null;
  resetPasswordStage: boolean = false;

  constructor(
    private userService: UserService,
    private route: ActivatedRoute,
    private router: Router
  ) { }

  ngOnInit(): void {
    this.token = this.route.snapshot.queryParamMap.get('token');
    this.email = this.route.snapshot.queryParamMap.get('email') || '';
    if (this.token) {
      this.resetPasswordStage = true;
    }
  }

  onResetPasswordRequest() {
    this.userService.forgotPassword(this.email).subscribe({
      next: (response) => {
        this.errorMessage = null;
        this.successMessage = 'Password reset email sent! Please check your email to reset your password.';
      },
      error: (error) => {
        this.successMessage = null;
        console.log("Erro completo:", error);
        this.errorMessage = error.error?.message || 'An error occurred when resetting the password.';
      }
    });
  }

  onResetPassword() {
    if (this.newPassword !== this.confirmPassword) {
      this.errorMessage = 'Passwords do not match';
      this.successMessage = null; // Garantir que não exista mensagem de sucesso
      return;
    }
  
    this.errorMessage = null;
    this.successMessage = null;
  
    console.log('Token:', this.token); // Debugging
    
    if (this.token) {
      this.userService.resetPassword(this.token, this.newPassword, this.email).subscribe({
        next: () => {
          this.errorMessage = null; // Limpar mensagem de erro em caso de sucesso
          this.successMessage = 'Password reset successfully!';
          
          // Adiciona um atraso para garantir que o usuário veja a mensagem de sucesso
          setTimeout(() => {
            this.router.navigate(['/login']); 
          }, 1500); // Redireciona após 1,5 segundos
        },
        error: (error) => {
          this.successMessage = null;
          this.errorMessage = 'Error resetting password. Please try again.';
        }
      });
    }
  }
  
}
