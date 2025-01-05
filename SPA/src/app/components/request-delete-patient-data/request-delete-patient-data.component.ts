import { Component, OnInit } from '@angular/core';
import { MarkXComponent } from '../template/mark-x/mark-x.component';
import { CommonModule } from '@angular/common';
import { ActivatedRoute, Router } from '@angular/router';
import { UserService } from '../../services/user.service';
import { AuthService } from '../../services/auth.service';
import { FormsModule } from '@angular/forms';

@Component({
  selector: 'app-request-delete-patient-data',
  standalone: true,
  imports: [MarkXComponent, CommonModule, FormsModule],
  templateUrl: './request-delete-patient-data.component.html',
  styleUrl: './request-delete-patient-data.component.css'
})
export class RequestDeletePatientDataComponent implements OnInit {
  email: string | null = null;
  successMessage: string = '';
  errorMessage: string = '';
  password: string = '';

  constructor(private route: ActivatedRoute, private userService: UserService, private authService: AuthService, private router: Router) { }

  ngOnInit(): void {
    this.email = this.route.snapshot.paramMap.get('email') || '';
    var emailVerified = this.authService.extractEmailFromToken();
    if (emailVerified != this.email) {
      this.router.navigate(['/home']);
    }
  }

  // Method triggered on delete confirmation
  async onDelete(): Promise<void> {
    if (this.email) {
      if (!this.password) {
        this.errorMessage = 'Password is required to make request to delete your data.';
        return;
      }

      const isValidPassword = await this.authService.validatePassword(this.email, this.password);
      if (!isValidPassword) {
        this.errorMessage = 'Invalid password. Please try again.';
        return;
      }

      this.userService.requestDeletePatientData(this.email).subscribe({
        next: (response: any) => {
          console.log('Sucessfull sent an email confirmation', response);
          this.successMessage = "An email will be sent to the data protection officer. You can close this window or click on the back button.";
        },
        error: (err: any) => {
          console.error('Failed to delete patient', err);
          this.errorMessage = 'Failed to delete your account.';
        }
      });
    }
  }

  onCancel(): void {
    history.back();
  }
}
