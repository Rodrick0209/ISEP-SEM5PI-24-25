import { CommonModule } from '@angular/common';
import { Component, EventEmitter, Output } from '@angular/core';
import { FormsModule } from '@angular/forms';

@Component({
  selector: 'app-filter-medical-record-entries',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './filter-medical-record-entries.component.html',
  styleUrl: './filter-medical-record-entries.component.css'
})
export class FilterMedicalRecordEntriesComponent {
  filter = {
    name: '',
  };

  @Output() filterChanged = new EventEmitter<{ name: string }>();

  applyFilter(): void {
    this.filterChanged.emit(this.filter);
  }
  
  onSearch(event: Event): void {
    event.preventDefault();
    this.applyFilter();
  }
}
