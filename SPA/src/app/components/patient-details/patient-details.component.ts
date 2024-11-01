import { Component, OnInit } from '@angular/core';
import { Patient, PatientService } from '../../services/patient.service';
import { ActivatedRoute } from '@angular/router';

@Component({
  selector: 'app-patient-details',
  standalone: true,
  imports: [],
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
    }
  }

  getPatientDetails(medicalRecordNumber: string): void {
    this.errorMessage = '';

    this.patientService.getPatient(medicalRecordNumber).subscribe({
      next: (data: Patient) => this.patient = data,
      error: (err: any) => {
        console.log('Failed to fetch patient details', err);
        this.errorMessage = 'Failed to fetch patient details';
      }
    });
  };

  
}
