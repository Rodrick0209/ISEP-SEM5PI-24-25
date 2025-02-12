import { Component, OnInit } from '@angular/core';
import { User, UserService } from '../../services/user.service';
import { ActivatedRoute, Route, Router } from '@angular/router';
import { MessageService } from '../../services/message.service';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-edit-user',
  standalone: true,
  imports: [FormsModule, CommonModule],
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
  showConfirmation: boolean = false;

  constructor(private userService: UserService, private route: ActivatedRoute, private router: Router, private messageService: MessageService) { }

  ngOnInit(): void {
    this.email = this.route.snapshot.paramMap.get('email');
  }

  confirmSubmission(): void {
    this.showConfirmation = true;
  }

  closeConfirmationModal(): void {
    this.showConfirmation = false;
  }

  
  editUser(userForm: any): void {
    this.showConfirmation = false;
    if (userForm.valid) {
      const userData = {
        ...this.submitForm
      };

      if (this.email) {
        var editUser = {
          email: this.email,
          nameToEdit: userData.name,
          emailToEdit: userData.email,
          phoneNumberToEdit: userData.phoneNumber
        };
        this.userService.edit(editUser).subscribe({
          next: (data: any) => {
            console.log('User updated successfully');
            if (this.submitForm.email || this.submitForm.phoneNumber) {
              this.successMessage = 'Please check your email to confirm the changes to your email and/or phone number.';
            } else {
              this.messageService.setMessage('User updated successfully');
              this.router.navigate(['/profile', this.email]);
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

  onCancel(): void {
    this.showConfirmation = false;
    history.back();
  }
}
