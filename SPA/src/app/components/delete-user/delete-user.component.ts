import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { CommonModule } from '@angular/common';
import { MarkXComponent } from '../template/mark-x/mark-x.component';
import { UserService } from '../../services/user.service';

@Component({
  selector: 'app-delete-user',
  standalone: true,
  imports: [CommonModule, MarkXComponent],
  templateUrl: './delete-user.component.html',
  styleUrl: './delete-user.component.css'
})
export class DeleteUserComponent implements OnInit {
  email: string | null = null;
  successMessage: string = '';
  errorMessage: string = '';

  constructor(private route: ActivatedRoute, private userService: UserService, private router: Router) { }
  
  ngOnInit(): void {
    this.email = this.route.snapshot.paramMap.get('email');
  }

  isConfirmed = false; // Track if the "X" mark has been clicked

  // Toggle the confirmation mark state
  onMarkClick(): void {
    this.isConfirmed = !this.isConfirmed;
  }

  // Method triggered on delete confirmation
  onDelete(): void {
    if (this.isConfirmed) {
      if (this.email) {
        this.userService.delete(this.email).subscribe({
          next: (response: any) => {
            console.log('Sucessfull sent an email confirmation', response);
            this.successMessage = "An email confirmation was sent to your inbox. Please confirm the deletion of your account and profile by clicking in the link.";
          },
          error: (err: any) => {
            console.error('Failed to delete patient', err);
            this.errorMessage = 'Failed to delete your account.';
          }
        });
      }
    } else {
      this.errorMessage = 'Please confirm by clicking in the mark.';
    }
  }

  onCancel(): void {
    this.router.navigate(['/profile', this.email]);
  }
}
