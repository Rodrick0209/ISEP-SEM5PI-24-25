import { Component, OnInit } from '@angular/core';
import { PatientService } from '../../services/patient.service';
import { ActivatedRoute } from '@angular/router';
import { CommonModule } from '@angular/common';
import { Patient } from '../../interfaces/patient';

@Component({
  selector: 'app-patient-details',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './patient-details.component.html',
  styleUrl: './patient-details.component.css'
})
export class PatientDetailsComponent implements OnInit {
  patient: Patient | undefined;
  errorMessage = '';

  constructor(private patientService: PatientService, private route: ActivatedRoute) { }

  ngOnInit(): void {
    const medicalRecordNumber = this.route.snapshot.paramMap.get('medicalRecordNumber');
    if (medicalRecordNumber) {
      this.getPatientDetails(medicalRecordNumber);
    } else {
      this.errorMessage = 'Invalid patient';
    }
  }

  getPatientDetails(medicalRecordNumber: string): void {
    this.errorMessage = '';

    this.patientService.getPatientByMedicalRecordNumber(medicalRecordNumber).subscribe({
      next: (data: Patient) => this.patient = data,
      error: (err: any) => {
        console.log('Failed to fetch patient details', err);
        this.errorMessage = 'Failed to fetch patient details';
      }
    });
  };


}
