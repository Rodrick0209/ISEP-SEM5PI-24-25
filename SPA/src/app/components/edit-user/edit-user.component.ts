import { Component, OnInit } from '@angular/core';
import { User, UserService } from '../../services/user.service';
import { ActivatedRoute, Route, Router } from '@angular/router';
import { MessageService } from '../../services/message.service';

@Component({
  selector: 'app-edit-user',
  standalone: true,
  imports: [],
  templateUrl: './edit-user.component.html',
  styleUrl: './edit-user.component.css'
})
export class EditUserComponent implements OnInit {
  user: User | undefined;
  submitForm = {
    name: '',
    email: '',
    phoneNumber: ''
  };
  email: string | null = null;
  errorMessage = '';
  successMessage = '';

  constructor(private userService: UserService, private route: ActivatedRoute, private router: Router, private messageService: MessageService) { }

  ngOnInit(): void {
    this.email = this.route.snapshot.paramMap.get('email');
  }

  onEdit(userForm: any): void {
    if (userForm.valid) {
      const userData = {
        ...this.submitForm
      };

      if (this.email) {
        this.userService.edit(
          this.email,
          userData.name,
          userData.email,
          userData.phoneNumber
        ).subscribe({
          next: (data: any) => {
            console.log('User updated successfully');
            if (this.submitForm.email || this.submitForm.phoneNumber) {
              this.successMessage = 'Please check your email to confirm the changes to your email and/or phone number.';
            } else {
              this.messageService.setMessage('User updated successfully');
              this.router.navigate(['/user-profile']);
            }
          },
          error: (err: any) => {
            console.error('Failed to update user', err);
            this.errorMessage = 'An error occurred while updating user: ' + err.error.message;
          }
        });
      } else {
        this.errorMessage = 'User not found';
      }
    }
  }
}
