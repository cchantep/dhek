package fr.applicius;

import java.io.FileOutputStream;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.BufferedImage;

import javax.swing.JScrollPane;
import javax.swing.JFrame;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;

final class Dhek {
    public static void main(String[] args) throws Exception {
        final String pdfPath = args[0];
        final float zoomRatio = Float.parseFloat(args[1]);

        System.out.println("Open PDF " + pdfPath + " with zoom " + zoomRatio);

        // ---

        final PDDocument doc = PDDocument.load(new java.io.File(pdfPath));
        final PDPage page = (PDPage) doc.getDocumentCatalog().getAllPages().get(0); // dest.getPage();
        final PdfComponent pdfComp = new PdfComponent(page, zoomRatio);

        final JFrame frm = new JFrame("Dhek");

        frm.setPreferredSize(new Dimension(640, 480));
        frm.getContentPane().add(new JScrollPane(pdfComp, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED));

        frm.pack();
        frm.setLocationRelativeTo(null);
        frm.setVisible(true);
        frm.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }
} // end of class Dhek

final class PdfComponent extends javax.swing.JComponent {
    final PDPage page;
    final Image img;
    final int iwidth;
    final int iheight;

    public PdfComponent(final PDPage p, float z) throws Exception {
        // TODO: setCropBox on page?
        this.page = p;
        BufferedImage i = p.convertToImage();

        final int w = i.getWidth();
        final int h = i.getHeight();

        this.iwidth = (int) (w * z);
        this.iheight = (int) (h * z);
        this.img = i.getScaledInstance(this.iwidth, this.iheight, 
                                       BufferedImage.SCALE_SMOOTH/*DEFAULT*/);

        final Dimension sz = new Dimension(this.iwidth, this.iheight);

        setPreferredSize(sz);
    }

    public void paint(Graphics g) {
        g.drawImage(this.img, 0, 0, this);
    }
}
