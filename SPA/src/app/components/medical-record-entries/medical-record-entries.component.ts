import { Component } from '@angular/core';
import { ListMedicalRecordEntriesComponent } from '../list-medical-record-entries/list-medical-record-entries.component';

@Component({
  selector: 'app-medical-record-entries',
  standalone: true,
  imports: [ListMedicalRecordEntriesComponent],
  templateUrl: './medical-record-entries.component.html',
  styleUrl: './medical-record-entries.component.css'
})
export class MedicalRecordEntriesComponent {
  medicalRecordEntries: string[] = [];

  onMedicalRecordEntryAdded(newMedicalRecordEntry: string) {
    this.medicalRecordEntries.push(newMedicalRecordEntry); // Adiciona novo registro médico à lista
  }
}
