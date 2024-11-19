import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { FormsModule } from '@angular/forms';

@Component({
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './planning.component.html',
  styleUrls: ['./planning.component.css']
})
export class PlanningComponent implements OnInit {
  date: string = '';
  selectedSala: string = '';
  greetingMessage: string = '';
  salasDeCirurgia = [
    { id: 1, nome: 'Room 1' },
    { id: 2, nome: 'Room 2' },
  ];

  // Variável para a data mínima (hoje)
  today: string = '';

  ngOnInit() {
    // Obtém a data atual no formato YYYY-MM-DD
    const currentDate = new Date();
    const year = currentDate.getFullYear();
    const month = (currentDate.getMonth() + 1).toString().padStart(2, '0');
    const day = currentDate.getDate().toString().padStart(2, '0');
    this.today = `${year}-${month}-${day}`;
  }

  agendarCirurgia() {
    if (this.date && this.selectedSala) {
      this.greetingMessage = `Surgery scheduled for ${this.date} in room ${this.selectedSala}`;
    } else {
      this.greetingMessage = 'Please fill in all fields before scheduling.';
    }
  }
}
