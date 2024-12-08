import { Component, OnInit } from '@angular/core';
import { User, UserService } from '../../services/user.service';
import { AuthService } from '../../services/auth.service';
import { ActivatedRoute, Router, RouterModule } from '@angular/router';
import { MessageService } from '../../services/message.service';
import { CommonModule } from '@angular/common';
import { PatientService } from '../../services/patient.service';

@Component({
  selector: 'app-user-profile',
  standalone: true,
  imports: [CommonModule, RouterModule],
  templateUrl: './user-profile.component.html',
  styleUrl: './user-profile.component.css'
})
export class UserProfileComponent implements OnInit {
  user: User | undefined;
  email: string | '' = '';
  medicalRecordNumber: string | null = null;
  successMessage: string | null = null;
  errorMessage: string | null = null; 

  constructor(private userService: UserService, private authService: AuthService, private route: ActivatedRoute, private messageService : MessageService, private router: Router, private patientService: PatientService) { }
  
  ngOnInit(): void {
    this.successMessage = this.messageService.getMessage();
    this.email = this.route.snapshot.paramMap.get('email') || '';
    if (this.email) {
      this.getUserByEmail(this.email);
    } else {
      this.errorMessage = 'Invalid user';
    }
    var emailVerified = this.authService.extractEmailFromToken();
    if (emailVerified != this.email) {
      this.router.navigate(['/home']);
    }
  }

  userSettings(): void {
    console.log('User settings clicked: ' + this.email);
    this.router.navigate(['/settings', this.email]);
  }

  viewPatientProfile(): void {
    this.patientService.getPatientByEmail(this.email).subscribe({
      next: (data) => {
        this.medicalRecordNumber = data.medicalRecordNumber;
        this.router.navigate(['/patient/details', this.medicalRecordNumber]);
      },
      error: (err) => {
        console.error('Error loading patient', err);
      }
    });
  }

  getUserByEmail(email: string): void {
    this.userService.getUserByEmail(email).subscribe({
      next: (data: User) => this.user = data,
      error: (err: any) => {
        console.log('Failed to fetch user details', err);
        this.errorMessage = 'Failed to fetch user details';
      }
    });
  };
}
