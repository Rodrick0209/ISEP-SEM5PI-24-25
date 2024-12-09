import { Component, OnInit } from '@angular/core';
import { PatientService } from '../../services/patient.service';
import { ActivatedRoute, Router } from '@angular/router';
import { CommonModule } from '@angular/common';
import { Patient } from '../../models/patient';
import { MedicalRecord } from '../../models/patient';
import { AuthService } from '../../services/auth.service';

@Component({
  selector: 'app-patient-details',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './patient-details.component.html',
  styleUrl: './patient-details.component.css'
})
export class PatientDetailsComponent implements OnInit {
  patient: Patient | undefined;
  medicalRecord: MedicalRecord | undefined;
  errorMessage = '';

  constructor(private patientService: PatientService, private route: ActivatedRoute, private authService: AuthService, private router: Router) { }

  ngOnInit(): void {
    const medicalRecordNumber = this.route.snapshot.paramMap.get('medicalRecordNumber');
    if (medicalRecordNumber) {
      this.getPatientDetails(medicalRecordNumber);
    } else {
      this.errorMessage = 'Invalid patient';
    }
  }

  extractRole(): any {
    return this.authService.extractRoleFromToken();
  }

  isDoctor(): boolean {
    const role = this.extractRole();
    return role === 'doctor';
  }

  isPatient(): boolean {
    const role = this.extractRole();
    return role === 'patient';
  }

  onMedicalRecord(): void {
    this.router.navigate(['/patient/medical-record', this.patient?.medicalRecordNumber]); // Adjust this route as needed
  }

  getPatientDetails(medicalRecordNumber: string): void {
    this.errorMessage = '';

   this.patientService.getPatientByMedicalRecordNumber(medicalRecordNumber).subscribe({
      next: (data: Patient) => {
          this.patient = data;
          if(this.isPatient()){
            var emailVerified = this.authService.extractEmailFromToken();
            if (emailVerified != this.patient?.email) {
              this.router.navigate(['/home']);
            }
          }
        },
      error: (err: any) => {
        console.log('Failed to fetch patient details', err);
        this.errorMessage = 'Failed to fetch patient details';
      }
    });
  }
}
