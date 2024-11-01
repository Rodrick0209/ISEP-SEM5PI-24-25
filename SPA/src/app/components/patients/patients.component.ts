import { Component } from '@angular/core';
import { ListPatientsComponent } from '../list-patients/list-patients.component';
import { CreatePatientComponent } from '../create-patient/create-patient.component';

@Component({
  selector: 'app-patients',
  standalone: true,
  imports: [ListPatientsComponent, CreatePatientComponent],
  templateUrl: './patients.component.html',
  styleUrl: './patients.component.css'
})
export class PatientsComponent {
  patients: string[] = [];

  onPatientAdded(newPatient: string) {
    this.patients.push(newPatient); // Adiciona novo paciente à lista
  }
}
